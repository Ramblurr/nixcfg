package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"net/netip"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/egress"
	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/policy"
)

// fileConfig is the JSON file named by -config. It holds no secret values:
// ca and onepassword name the systemd credentials that do.
type fileConfig struct {
	TCPListen        []string       `json:"tcpListen"`
	UDPListen        []string       `json:"udpListen"`
	TailscaledSocket string         `json:"tailscaledSocket"`
	DNSUpstream      string         `json:"dnsUpstream"`
	Self             []string       `json:"self"`
	IdentityTTL      duration       `json:"identityTTL"`
	PeekTimeout      duration       `json:"peekTimeout"`
	UDPIdle          duration       `json:"udpIdle"`
	Helper           *helperConfig  `json:"helper"`
	CA               *caConfig      `json:"ca"`
	OnePassword      *onePassConfig `json:"onepassword"`
	BrokerSocket     string         `json:"brokerSocket"`
}

// helperConfig enables the Tailscale CONNECT helper.
type helperConfig struct {
	Listen string   `json:"listen"`
	Hosts  []string `json:"hosts"`
	Ports  []uint16 `json:"ports"`
}

// caConfig enables decryption of flows that match an http rule.
type caConfig struct {
	CertCredential string `json:"certCredential"`
	KeyCredential  string `json:"keyCredential"`
}

// onePassConfig lets http rules inject op:// secrets through Connect.
type onePassConfig struct {
	URL             string   `json:"url"`
	TokenCredential string   `json:"tokenCredential"`
	SecretTTL       duration `json:"secretTTL"`
}

// duration is a time.Duration written as a Go duration string, e.g. "15s".
type duration time.Duration

func (d *duration) UnmarshalJSON(b []byte) error {
	var s string
	if err := json.Unmarshal(b, &s); err != nil {
		return errors.New("duration must be a string such as \"15s\"")
	}
	v, err := time.ParseDuration(s)
	if err != nil {
		return err
	}
	if v <= 0 {
		return fmt.Errorf("duration %q must be positive", s)
	}
	*d = duration(v)
	return nil
}

type config struct {
	tcpListen   []string
	udpListen   []string
	socket      string
	resolver    string
	self        []netip.Addr
	identityTTL time.Duration
	peekTimeout time.Duration
	udpIdle     time.Duration
	helper      string
	helperHosts []policy.HostPattern
	helperPorts []uint16
	// caCert and caKey are credential names; empty disables decryption.
	caCert, caKey string
	// opURL empty means only literal: secrets resolve.
	opURL, opToken string
	secretTTL      time.Duration
	// brokerSocket empty means no placeholder outside the rules resolves.
	brokerSocket string
}

func defaultFileConfig() fileConfig {
	// Separate slices: decoding into one reuses its backing array.
	return fileConfig{
		TCPListen:        []string{"0.0.0.0:15001", "[::]:15001"},
		UDPListen:        []string{"0.0.0.0:15001", "[::]:15001"},
		TailscaledSocket: "/var/run/tailscale/tailscaled.sock",
		IdentityTTL:      duration(15 * time.Second),
		PeekTimeout:      duration(5 * time.Second),
		UDPIdle:          duration(time.Minute),
	}
}

// parseConfig reads the config file strictly: unknown keys and anything
// after the top-level object are errors.
func parseConfig(data []byte) (config, error) {
	f := defaultFileConfig()
	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&f); err != nil {
		return config{}, err
	}
	if _, err := dec.Token(); !errors.Is(err, io.EOF) {
		return config{}, errors.New("trailing data after the config object")
	}

	c := config{
		tcpListen:   f.TCPListen,
		udpListen:   f.UDPListen,
		socket:      f.TailscaledSocket,
		resolver:    f.DNSUpstream,
		identityTTL: time.Duration(f.IdentityTTL),
		peekTimeout: time.Duration(f.PeekTimeout),
		udpIdle:     time.Duration(f.UDPIdle),
	}
	if c.resolver == "" {
		return c, errors.New("dnsUpstream is required")
	}
	if _, _, err := net.SplitHostPort(c.resolver); err != nil {
		return c, fmt.Errorf("dnsUpstream: %w", err)
	}
	for _, s := range f.Self {
		a, err := netip.ParseAddr(s)
		if err != nil {
			return c, fmt.Errorf("self: %w", err)
		}
		c.self = append(c.self, a)
	}
	if h := f.Helper; h != nil {
		if h.Listen == "" {
			return c, errors.New("helper.listen is required")
		}
		c.helper = h.Listen
		hosts := h.Hosts
		if len(hosts) == 0 {
			hosts = egress.DefaultHelperHosts
		}
		for _, s := range hosts {
			p, err := policy.ParseHostPattern(s)
			if err != nil {
				return c, fmt.Errorf("helper.hosts: %w", err)
			}
			c.helperHosts = append(c.helperHosts, p)
		}
		c.helperPorts = h.Ports
		if len(c.helperPorts) == 0 {
			c.helperPorts = []uint16{80, 443}
		}
		for _, p := range c.helperPorts {
			if p == 0 {
				return c, errors.New("helper.ports: port 0")
			}
		}
	}
	if ca := f.CA; ca != nil {
		c.caCert = orDefault(ca.CertCredential, "ca-cert")
		c.caKey = orDefault(ca.KeyCredential, "ca-key")
	}
	if op := f.OnePassword; op != nil {
		if f.CA == nil {
			return c, errors.New("onepassword needs ca: secrets are only injected into decrypted flows")
		}
		if op.URL == "" {
			return c, errors.New("onepassword.url is required")
		}
		c.opURL = op.URL
		c.opToken = orDefault(op.TokenCredential, "op-connect-token")
		c.secretTTL = time.Duration(op.SecretTTL)
	}
	if f.BrokerSocket != "" {
		if f.OnePassword == nil {
			return c, errors.New("brokerSocket needs onepassword: broker credentials are op:// secrets")
		}
		if !filepath.IsAbs(f.BrokerSocket) {
			return c, errors.New("brokerSocket must be an absolute path")
		}
		c.brokerSocket = f.BrokerSocket
	}
	if c.secretTTL == 0 {
		c.secretTTL = time.Minute
	}
	for _, name := range []string{c.caCert, c.caKey, c.opToken} {
		if name != "" && (!credentialName.MatchString(name) || name == "." || name == "..") {
			return c, fmt.Errorf("credential name %q is not a plain file name", name)
		}
	}
	return c, nil
}

func orDefault(s, def string) string {
	if s == "" {
		return def
	}
	return s
}

var credentialName = regexp.MustCompile(`^[A-Za-z0-9_.-]+$`)

// readCredential returns the systemd credential name from dir, normally
// $CREDENTIALS_DIRECTORY. A missing directory, file or value is an error.
func readCredential(dir, name string) ([]byte, error) {
	if dir == "" {
		return nil, fmt.Errorf("credential %s: $CREDENTIALS_DIRECTORY is not set", name)
	}
	b, err := os.ReadFile(filepath.Join(dir, name))
	if err != nil {
		return nil, fmt.Errorf("credential %s: %w", name, err)
	}
	if strings.TrimSpace(string(b)) == "" {
		return nil, fmt.Errorf("credential %s is empty", name)
	}
	return b, nil
}
