// Command ts-egress is a Tailscale exit node of its own, built on tsnet. It
// decides every flow a tenant sends through it from the caller's
// moot.land/cap/egress grant capabilities and denies anything they do not
// allow.
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"log/slog"
	"net"
	"net/http"
	"net/netip"
	"os"
	"os/signal"
	"strings"
	"syscall"
	"time"

	"tailscale.com/tsnet"

	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/broker"
	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/egress"
	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/policy"
	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/secrets"
)

type options struct {
	config   string
	logLevel slog.Level
}

func parseFlags(args []string) (options, error) {
	fs := flag.NewFlagSet("ts-egress", flag.ContinueOnError)
	var o options
	fs.StringVar(&o.config, "config", "", "JSON config file (required)")
	level := fs.String("log-level", "info", "log level for diagnostics: debug, info, warn, error")
	if err := fs.Parse(args); err != nil {
		return o, err
	}
	if fs.NArg() != 0 {
		return o, fmt.Errorf("unexpected arguments: %q", fs.Args())
	}
	if o.config == "" {
		return o, errors.New("-config is required")
	}
	return o, o.logLevel.UnmarshalText([]byte(*level))
}

func interfaceAddrs() ([]netip.Addr, error) {
	addrs, err := net.InterfaceAddrs()
	if err != nil {
		return nil, err
	}
	var out []netip.Addr
	for _, a := range addrs {
		if p, err := netip.ParsePrefix(a.String()); err == nil {
			out = append(out, p.Addr())
		}
	}
	return out, nil
}

func run(ctx context.Context, c config, credDir string) error {
	audit := slog.New(slog.NewJSONHandler(os.Stdout, nil))

	own, err := interfaceAddrs()
	if err != nil {
		return err
	}
	floor := policy.NewFloor(append(own, c.self...))
	names := egress.NewNames()
	srv := &egress.Server{
		Floor:       floor,
		Names:       names,
		Resolver:    c.resolver,
		PeekTimeout: c.peekTimeout,
		UDPIdle:     c.udpIdle,
		Audit:       audit,
	}
	authKey, err := readCredential(credDir, c.authKey)
	if err != nil {
		return err
	}

	ts := &tsnet.Server{
		Dir:           c.stateDir,
		Hostname:      c.hostname,
		AuthKey:       strings.TrimSpace(string(authKey)),
		AdvertiseTags: c.tags,
		Port:          c.port,
		UserLogf:      tsnetLog(slog.LevelInfo),
		Logf:          tsnetLog(slog.LevelDebug),
	}
	defer ts.Close()
	if c.caCert != "" {
		d, err := decrypter(c, credDir, audit, ts.HTTPClient())
		if err != nil {
			return err
		}
		srv.Decrypter = d
	}
	go func() {
		t := time.NewTicker(time.Minute)
		defer t.Stop()
		for {
			select {
			case <-ctx.Done():
				return
			case <-t.C:
				names.Sweep()
			}
		}
	}()
	if err := srv.ExitNode(ctx, ts, c.identityTTL); err != nil {
		return fmt.Errorf("tailnet: %w", err)
	}

	errc := make(chan error, 1)
	var closers []func() error
	if c.helper != "" {
		hs := &http.Server{
			Addr:              c.helper,
			Handler:           &egress.Helper{Hosts: c.helperHosts, Ports: c.helperPorts, Floor: floor, Audit: audit},
			ReadHeaderTimeout: 10 * time.Second,
		}
		closers = append(closers, hs.Close)
		go func() {
			if err := hs.ListenAndServe(); !errors.Is(err, http.ErrServerClosed) {
				errc <- err
			}
		}()
	}
	slog.Info("ts-egress started", "hostname", c.hostname, "helper", c.helper, "dns_upstream", c.resolver, "decrypt", srv.Decrypter != nil, "broker", c.brokerSocket)

	select {
	case <-ctx.Done():
		err = nil
	case err = <-errc:
	}
	for _, cl := range closers {
		_ = cl()
	}
	return err
}

func tsnetLog(level slog.Level) func(string, ...any) {
	return func(format string, args ...any) {
		slog.Log(context.Background(), level, fmt.Sprintf(format, args...), "from", "tsnet")
	}
}

// decrypter loads the CA and, when 1Password is configured, the Connect
// token from the systemd credentials in credDir. Without 1Password only
// literal: secrets resolve.
func decrypter(c config, credDir string, audit *slog.Logger, client *http.Client) (*egress.HTTPDecrypter, error) {
	certPEM, err := readCredential(credDir, c.caCert)
	if err != nil {
		return nil, err
	}
	keyPEM, err := readCredential(credDir, c.caKey)
	if err != nil {
		return nil, err
	}
	leaves, err := egress.NewLeaves(certPEM, keyPEM)
	if err != nil {
		return nil, err
	}
	store := &secrets.Store{TTL: c.secretTTL}
	if c.opURL != "" {
		tok, err := readCredential(credDir, c.opToken)
		if err != nil {
			return nil, err
		}
		store.OP = &secrets.Connect{Host: c.opURL, Token: strings.TrimSpace(string(tok)), Client: client}
	}
	d := &egress.HTTPDecrypter{Leaves: leaves, Secrets: store, Audit: audit}
	if c.brokerSocket != "" {
		d.Broker = &broker.Client{Socket: c.brokerSocket}
	}
	return d, nil
}

// load parses the flags and the config file they name.
func load(args []string) (options, config, error) {
	o, err := parseFlags(args)
	if err != nil {
		return o, config{}, err
	}
	data, err := os.ReadFile(o.config)
	if err != nil {
		return o, config{}, err
	}
	c, err := parseConfig(data)
	if err != nil {
		return o, c, fmt.Errorf("%s: %w", o.config, err)
	}
	return o, c, nil
}

func main() {
	o, c, err := load(os.Args[1:])
	if errors.Is(err, flag.ErrHelp) {
		return
	}
	if err != nil {
		fmt.Fprintln(os.Stderr, "ts-egress:", err)
		os.Exit(2)
	}
	slog.SetDefault(slog.New(slog.NewJSONHandler(os.Stderr, &slog.HandlerOptions{Level: o.logLevel})))
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer stop()
	if err := run(ctx, c, os.Getenv("CREDENTIALS_DIRECTORY")); err != nil {
		slog.Error("ts-egress stopped", "err", err)
		os.Exit(1)
	}
}
