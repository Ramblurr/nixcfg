package main

import (
	"crypto/ecdsa"
	"crypto/elliptic"
	"crypto/rand"
	"crypto/x509"
	"crypto/x509/pkix"
	"encoding/pem"
	"io"
	"log/slog"
	"math/big"
	"net/netip"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/secrets"
)

const fullConfig = `{
  "hostname": "egress-test",
  "stateDir": "/var/lib/egress-test",
  "authKeyCredential": "authkey",
  "tags": ["tag:egress"],
  "port": 41641,
  "dnsUpstream": "192.0.2.1:53",
  "self": ["192.0.2.26"],
  "identityTTL": "30s",
  "peekTimeout": "2s",
  "udpIdle": "90s",
  "helper": {"listen": "192.0.2.26:3128", "hosts": ["controlplane.tailscale.com"], "ports": [443]},
  "ca": {"certCredential": "cert", "keyCredential": "key"},
  "onepassword": {"url": "http://op-connect:8080", "tokenCredential": "tok", "secretTTL": "5m"},
  "brokerSocket": "/run/ts-broker/decide.sock"
}`

func TestParseConfigFull(t *testing.T) {
	c, err := parseConfig([]byte(fullConfig))
	if err != nil {
		t.Fatal(err)
	}
	if c.hostname != "egress-test" || c.stateDir != "/var/lib/egress-test" || c.authKey != "authkey" || !slices.Equal(c.tags, []string{"tag:egress"}) || c.port != 41641 {
		t.Errorf("node = %q %q %q %v %d", c.hostname, c.stateDir, c.authKey, c.tags, c.port)
	}
	if c.resolver != "192.0.2.1:53" {
		t.Errorf("resolver %q", c.resolver)
	}
	if !slices.Equal(c.self, []netip.Addr{netip.MustParseAddr("192.0.2.26")}) {
		t.Errorf("self = %v", c.self)
	}
	if c.identityTTL != 30*time.Second || c.peekTimeout != 2*time.Second || c.udpIdle != 90*time.Second || c.secretTTL != 5*time.Minute {
		t.Errorf("durations = %v %v %v %v", c.identityTTL, c.peekTimeout, c.udpIdle, c.secretTTL)
	}
	if c.helper != "192.0.2.26:3128" || len(c.helperHosts) != 1 || !slices.Equal(c.helperPorts, []uint16{443}) {
		t.Errorf("helper = %q %v %v", c.helper, c.helperHosts, c.helperPorts)
	}
	if c.caCert != "cert" || c.caKey != "key" || c.opURL != "http://op-connect:8080" || c.opToken != "tok" {
		t.Errorf("credentials = %q %q %q %q", c.caCert, c.caKey, c.opURL, c.opToken)
	}
	if c.brokerSocket != "/run/ts-broker/decide.sock" {
		t.Errorf("brokerSocket = %q", c.brokerSocket)
	}
}

func TestParseConfigDefaults(t *testing.T) {
	c, err := parseConfig([]byte(`{"dnsUpstream": "192.0.2.1:53", "helper": {"listen": ":3128"}, "ca": {}, "onepassword": {"url": "http://op:8080"}}`))
	if err != nil {
		t.Fatal(err)
	}
	if c.hostname != "egress" || c.stateDir != "/var/lib/ts-egress" || c.authKey != "ts-authkey" || c.tags != nil || c.port != 0 {
		t.Errorf("node = %q %q %q %v %d", c.hostname, c.stateDir, c.authKey, c.tags, c.port)
	}
	if c.identityTTL != 15*time.Second || c.peekTimeout != 5*time.Second || c.udpIdle != time.Minute || c.secretTTL != time.Minute {
		t.Errorf("durations = %v %v %v %v", c.identityTTL, c.peekTimeout, c.udpIdle, c.secretTTL)
	}
	if len(c.helperHosts) == 0 || !slices.Equal(c.helperPorts, []uint16{80, 443}) {
		t.Errorf("helper = %v %v", c.helperHosts, c.helperPorts)
	}
	if c.caCert != "ca-cert" || c.caKey != "ca-key" || c.opToken != "op-connect-token" {
		t.Errorf("credential names = %q %q %q", c.caCert, c.caKey, c.opToken)
	}
}

func TestParseConfigMinimalDisablesOptionalParts(t *testing.T) {
	c, err := parseConfig([]byte(`{"dnsUpstream": "192.0.2.1:53"}`))
	if err != nil {
		t.Fatal(err)
	}
	if c.helper != "" || c.caCert != "" || c.opURL != "" || c.brokerSocket != "" {
		t.Errorf("helper %q ca %q op %q broker %q, want all off", c.helper, c.caCert, c.opURL, c.brokerSocket)
	}
}

func TestParseConfigRejects(t *testing.T) {
	for name, tc := range map[string]struct{ json, want string }{
		"unknown key":           {`{"dnsUpstream": "192.0.2.1:53", "caKey": "/run/secrets/ca_key"}`, `unknown field "caKey"`},
		"unknown nested key":    {`{"dnsUpstream": "192.0.2.1:53", "ca": {"keyFile": "/x"}}`, `unknown field "keyFile"`},
		"trailing object":       {`{"dnsUpstream": "192.0.2.1:53"} {}`, "trailing data"},
		"trailing garbage":      {`{"dnsUpstream": "192.0.2.1:53"} x`, "trailing data"},
		"empty":                 {``, "EOF"},
		"empty hostname":        {`{"dnsUpstream": "192.0.2.1:53", "hostname": ""}`, "hostname must not be empty"},
		"relative stateDir":     {`{"dnsUpstream": "192.0.2.1:53", "stateDir": "state"}`, "stateDir must be an absolute path"},
		"empty auth key":        {`{"dnsUpstream": "192.0.2.1:53", "authKeyCredential": ""}`, "authKeyCredential must not be empty"},
		"auth key path":         {`{"dnsUpstream": "192.0.2.1:53", "authKeyCredential": "../key"}`, "not a plain file name"},
		"port out of range":     {`{"dnsUpstream": "192.0.2.1:53", "port": 70000}`, "cannot unmarshal"},
		"no dnsUpstream":        {`{}`, "dnsUpstream is required"},
		"bad dnsUpstream":       {`{"dnsUpstream": "192.0.2.1"}`, "dnsUpstream"},
		"bad self":              {`{"dnsUpstream": "192.0.2.1:53", "self": ["nope"]}`, "self"},
		"bad duration":          {`{"dnsUpstream": "192.0.2.1:53", "udpIdle": "soon"}`, "invalid duration"},
		"numeric duration":      {`{"dnsUpstream": "192.0.2.1:53", "udpIdle": 60}`, "duration must be a string"},
		"zero duration":         {`{"dnsUpstream": "192.0.2.1:53", "peekTimeout": "0s"}`, "must be positive"},
		"helper without listen": {`{"dnsUpstream": "192.0.2.1:53", "helper": {}}`, "helper.listen is required"},
		"helper port zero":      {`{"dnsUpstream": "192.0.2.1:53", "helper": {"listen": ":3128", "ports": [0]}}`, "port 0"},
		"onepassword alone":     {`{"dnsUpstream": "192.0.2.1:53", "onepassword": {"url": "http://op:8080"}}`, "onepassword needs ca"},
		"onepassword no url":    {`{"dnsUpstream": "192.0.2.1:53", "ca": {}, "onepassword": {}}`, "onepassword.url is required"},
		"credential path":       {`{"dnsUpstream": "192.0.2.1:53", "ca": {"keyCredential": "../ca-key"}}`, "not a plain file name"},
		"credential dotdot":     {`{"dnsUpstream": "192.0.2.1:53", "ca": {"keyCredential": ".."}}`, "not a plain file name"},
		"broker alone":          {`{"dnsUpstream": "192.0.2.1:53", "ca": {}, "brokerSocket": "/run/b.sock"}`, "brokerSocket needs onepassword"},
		"broker relative":       {`{"dnsUpstream": "192.0.2.1:53", "ca": {}, "onepassword": {"url": "http://op:8080"}, "brokerSocket": "b.sock"}`, "absolute path"},
	} {
		t.Run(name, func(t *testing.T) {
			_, err := parseConfig([]byte(tc.json))
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("err = %v, want it to mention %q", err, tc.want)
			}
		})
	}
}

func TestParseFlags(t *testing.T) {
	if _, err := parseFlags(nil); err == nil || !strings.Contains(err.Error(), "-config is required") {
		t.Errorf("no -config: err = %v", err)
	}
	if _, err := parseFlags([]string{"-config", "c.json", "-dns-upstream", "x"}); err == nil {
		t.Error("an old flag was accepted")
	}
	if _, err := parseFlags([]string{"-config", "c.json", "extra"}); err == nil {
		t.Error("a positional argument was accepted")
	}
	o, err := parseFlags([]string{"-config", "c.json", "-log-level", "debug"})
	if err != nil || o.config != "c.json" || o.logLevel != slog.LevelDebug {
		t.Errorf("parseFlags = %+v, %v", o, err)
	}
}

// writeCA writes a fresh CA certificate and key as the credentials
// ca-cert and ca-key in dir.
func writeCA(t *testing.T, dir string) {
	t.Helper()
	key, err := ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
	if err != nil {
		t.Fatal(err)
	}
	tmpl := &x509.Certificate{
		SerialNumber:          big.NewInt(1),
		Subject:               pkix.Name{CommonName: "test CA"},
		NotBefore:             time.Now().Add(-time.Hour),
		NotAfter:              time.Now().Add(time.Hour),
		IsCA:                  true,
		BasicConstraintsValid: true,
		KeyUsage:              x509.KeyUsageCertSign,
	}
	der, err := x509.CreateCertificate(rand.Reader, tmpl, tmpl, &key.PublicKey, key)
	if err != nil {
		t.Fatal(err)
	}
	keyDER, err := x509.MarshalPKCS8PrivateKey(key)
	if err != nil {
		t.Fatal(err)
	}
	write(t, dir, "ca-cert", string(pem.EncodeToMemory(&pem.Block{Type: "CERTIFICATE", Bytes: der})))
	write(t, dir, "ca-key", string(pem.EncodeToMemory(&pem.Block{Type: "PRIVATE KEY", Bytes: keyDER})))
}

func write(t *testing.T, dir, name, value string) {
	t.Helper()
	if err := os.WriteFile(filepath.Join(dir, name), []byte(value), 0o600); err != nil {
		t.Fatal(err)
	}
}

func TestDecrypterReadsCredentials(t *testing.T) {
	c, err := parseConfig([]byte(`{"dnsUpstream": "192.0.2.1:53", "ca": {}, "onepassword": {"url": "http://op:8080"}, "brokerSocket": "/run/b.sock"}`))
	if err != nil {
		t.Fatal(err)
	}
	audit := slog.New(slog.NewJSONHandler(io.Discard, nil))
	dir := t.TempDir()
	writeCA(t, dir)
	write(t, dir, "op-connect-token", "token\n")
	d, err := decrypter(c, dir, audit)
	if err != nil {
		t.Fatal(err)
	}
	store, ok := d.Secrets.(*secrets.Store)
	if d.Leaves == nil || !ok || store.OP == nil || d.Broker == nil || d.Broker.Socket != "/run/b.sock" {
		t.Fatalf("decrypter = %+v, want leaves, a Connect fetcher and the broker", d)
	}
}

func TestDecrypterFailsClosed(t *testing.T) {
	c, err := parseConfig([]byte(`{"dnsUpstream": "192.0.2.1:53", "ca": {}, "onepassword": {"url": "http://op:8080"}}`))
	if err != nil {
		t.Fatal(err)
	}
	audit := slog.New(slog.NewJSONHandler(io.Discard, nil))
	for name, tc := range map[string]struct {
		setup func(t *testing.T, dir string)
		dir   func(dir string) string
		want  string
	}{
		"no credentials directory": {
			setup: writeCA,
			dir:   func(string) string { return "" },
			want:  "$CREDENTIALS_DIRECTORY is not set",
		},
		"missing CA key": {
			setup: func(t *testing.T, dir string) { writeCA(t, dir); os.Remove(filepath.Join(dir, "ca-key")) },
			want:  "credential ca-key",
		},
		"missing Connect token": {
			setup: writeCA,
			want:  "credential op-connect-token",
		},
		"empty Connect token": {
			setup: func(t *testing.T, dir string) { writeCA(t, dir); write(t, dir, "op-connect-token", " \n") },
			want:  "credential op-connect-token is empty",
		},
		"empty CA certificate": {
			setup: func(t *testing.T, dir string) { writeCA(t, dir); write(t, dir, "ca-cert", "") },
			want:  "credential ca-cert is empty",
		},
	} {
		t.Run(name, func(t *testing.T) {
			dir := t.TempDir()
			tc.setup(t, dir)
			if tc.dir != nil {
				dir = tc.dir(dir)
			}
			if _, err := decrypter(c, dir, audit); err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("err = %v, want it to mention %q", err, tc.want)
			}
		})
	}
}
