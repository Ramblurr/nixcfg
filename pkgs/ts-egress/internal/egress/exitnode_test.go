package egress

import (
	"bytes"
	"context"
	"crypto/tls"
	"crypto/x509"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"testing"
	"time"

	"golang.org/x/net/dns/dnsmessage"
	"tailscale.com/ipn"
	"tailscale.com/ipn/store/mem"
	"tailscale.com/net/netns"
	"tailscale.com/net/tsaddr"
	"tailscale.com/tailcfg"
	"tailscale.com/tsnet"
	"tailscale.com/tstest/integration"
	"tailscale.com/tstest/integration/testcontrol"
	"tailscale.com/types/logger"

	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/policy"
)

const exitNodeCaps = `{
	"tcp": [{"cidrs": ["198.51.100.5/32"], "ports": [7000]}],
	"udp": [{"cidrs": ["198.51.100.5/32"], "ports": [7002]}],
	"tls": [{"hosts": ["svc.test"], "ports": [443]}],
	"http": [{"hosts": ["example.com"], "ports": [8443], "credentials": [
		{"name": "DUMMY_TOKEN", "secret": "literal:egress-dummy-value", "inject": {"header": "X-Dummy-Token", "format": "{secret}"}}
	]}],
	"dns": {"allow": "rules"}
}`

// TestExitNode runs a tagged tenant through a tsnet egress on an offline
// tailnet: testcontrol and an in-process DERP. The egress dials "internet"
// addresses through Server.Dial, which maps them to loopback listeners.
func TestExitNode(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()

	control := testControl(t)
	control.SetGlobalAppCaps(tailcfg.PeerCapMap{policy.CapName: {tailcfg.RawMessage(exitNodeCaps)}})

	tlsSrv := httptest.NewTLSServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		_, _ = io.WriteString(w, "token="+r.Header.Get("X-Dummy-Token"))
	}))
	t.Cleanup(tlsSrv.Close)
	internet := map[string]string{
		"tcp 198.51.100.5:7000": loopbackEcho(t, "tcp"),
		"tcp 198.51.100.5:7001": loopbackEcho(t, "tcp"),
		"udp 198.51.100.5:7002": loopbackEcho(t, "udp"),
		"udp 198.51.100.5:7004": loopbackEcho(t, "udp"),
		"tcp example.com:8443":  tlsSrv.Listener.Addr().String(),
	}
	resolver, _ := fakeResolver(t, map[string][4]byte{"svc.test.": {198, 51, 100, 5}})
	s, ab := newServer(nil, resolver)
	s.Dial = func(*Caller) DialFunc {
		return func(ctx context.Context, network, address string) (net.Conn, error) {
			target, ok := internet[network+" "+address]
			if !ok {
				return nil, fmt.Errorf("no route to %s %s", network, address)
			}
			return (&net.Dialer{}).DialContext(ctx, network, target)
		}
	}
	leaves, tenantRoots := testCA(t)
	upstreamRoots := x509.NewCertPool()
	upstreamRoots.AddCert(tlsSrv.Certificate())
	s.Decrypter = &HTTPDecrypter{Leaves: leaves, Secrets: fakeSecrets{}, RootCAs: upstreamRoots, Audit: s.Audit}

	egress := tsnetNode(t, control, "egress", "tag:egress")
	if err := s.ExitNode(ctx, egress, time.Minute); err != nil {
		t.Fatal(err)
	}
	elc, err := egress.LocalClient()
	if err != nil {
		t.Fatal(err)
	}
	est, err := elc.Status(ctx)
	if err != nil {
		t.Fatal(err)
	}
	control.SetSubnetRoutes(est.Self.PublicKey, tsaddr.ExitRoutes())

	alice := tsnetNode(t, control, "alice", "tag:t3-alice")
	if _, err := alice.Up(ctx); err != nil {
		t.Fatal(err)
	}
	alc, err := alice.LocalClient()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := alc.EditPrefs(ctx, &ipn.MaskedPrefs{
		Prefs:         ipn.Prefs{ExitNodeID: control.Node(est.Self.PublicKey).StableID},
		ExitNodeIDSet: true,
	}); err != nil {
		t.Fatal(err)
	}
	var peerAPI string
	for {
		st, err := alc.Status(ctx)
		if err != nil {
			t.Fatal(err)
		}
		if p := st.Peer[est.Self.PublicKey]; p != nil && p.ExitNode && p.PrimaryRoutes != nil && p.PrimaryRoutes.Len() > 0 && len(p.PeerAPIURL) > 0 {
			peerAPI = p.PeerAPIURL[0]
			break
		}
		if ctx.Err() != nil {
			t.Fatal("alice never got the egress as its exit node")
		}
		time.Sleep(100 * time.Millisecond)
	}

	auditDeny := func(t *testing.T, dst string) {
		t.Helper()
		for deadline := time.Now().Add(3 * time.Second); time.Now().Before(deadline); time.Sleep(20 * time.Millisecond) {
			for _, l := range ab.lines(t) {
				if l["msg"] == "flow" && l["dst"] == dst && l["verdict"] == "deny" && l["node"] == "alice" {
					return
				}
			}
		}
		t.Fatalf("no deny audit line for alice to %s: %v", dst, ab.lines(t))
	}

	t.Run("tcp allowed", func(t *testing.T) {
		if got := tenantTCP(t, ctx, alice, "198.51.100.5:7000", "ping"); got != "ping" {
			t.Fatalf("echo = %q", got)
		}
	})
	t.Run("tcp denied", func(t *testing.T) {
		if got := tenantTCP(t, ctx, alice, "198.51.100.5:7001", "ping"); got != "" {
			t.Fatalf("denied flow echoed %q", got)
		}
		auditDeny(t, "198.51.100.5:7001")
	})
	t.Run("http decrypts and injects", func(t *testing.T) {
		tr := &http.Transport{
			DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
				return alice.Dial(ctx, "tcp", "203.0.113.9:8443")
			},
			TLSClientConfig: &tls.Config{RootCAs: tenantRoots},
		}
		defer tr.CloseIdleConnections()
		req, _ := http.NewRequestWithContext(ctx, "GET", "https://example.com:8443/", nil)
		req.Header.Set("X-Dummy-Token", policy.PlaceholderPrefix+"DUMMY_TOKEN")
		res, err := (&http.Client{Transport: tr, Timeout: 10 * time.Second}).Do(req)
		if err != nil {
			t.Fatal(err)
		}
		defer res.Body.Close()
		body, _ := io.ReadAll(res.Body)
		if string(body) != "token=egress-dummy-value" {
			t.Fatalf("upstream saw %q", body)
		}
	})
	t.Run("udp allowed", func(t *testing.T) {
		if got := tenantUDP(t, ctx, alice, "198.51.100.5:7002", []byte("ping")); string(got) != "ping" {
			t.Fatalf("echo = %q", got)
		}
	})
	t.Run("udp denied", func(t *testing.T) {
		if got := tenantUDP(t, ctx, alice, "198.51.100.5:7004", []byte("ping")); got != nil {
			t.Fatalf("denied flow echoed %q", got)
		}
		auditDeny(t, "198.51.100.5:7004")
	})
	t.Run("dns on port 53 follows the rules", func(t *testing.T) {
		for name, want := range map[string]dnsmessage.RCode{"svc.test.": dnsmessage.RCodeSuccess, "other.test.": dnsmessage.RCodeRefused} {
			resp := tenantUDP(t, ctx, alice, "203.0.113.53:53", query(t, name))
			var m dnsmessage.Message
			if err := m.Unpack(resp); err != nil {
				t.Fatalf("%s: %v", name, err)
			}
			if m.RCode != want {
				t.Errorf("%s: rcode = %v, want %v", name, m.RCode, want)
			}
		}
	})
	t.Run("peerapi doh is refused", func(t *testing.T) {
		req, _ := http.NewRequestWithContext(ctx, "GET", peerAPI+"/dns-query?q=svc.test", nil)
		res, err := alice.HTTPClient().Do(req)
		if err != nil {
			t.Fatal(err)
		}
		res.Body.Close()
		if res.StatusCode != http.StatusForbidden {
			t.Fatalf("doh status = %d, want 403", res.StatusCode)
		}
	})
}

func TestExitNodeRefusesAStartedServer(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), time.Minute)
	defer cancel()
	egress := tsnetNode(t, testControl(t), "egress", "tag:egress")
	if _, err := egress.LocalClient(); err != nil {
		t.Fatal(err)
	}
	s, _ := newServer(nil, "")
	if err := s.ExitNode(ctx, egress, time.Minute); err == nil {
		t.Fatal("ExitNode took a tsnet server that was already started")
	}
}

func TestExitNodeRefusesAnUntaggedNode(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), time.Minute)
	defer cancel()
	egress := tsnetNode(t, testControl(t), "egress")
	s, _ := newServer(nil, "")
	if err := s.ExitNode(ctx, egress, time.Minute); err == nil {
		t.Fatal("ExitNode advertised exit routes from an untagged node")
	}
	lc, err := egress.LocalClient()
	if err != nil {
		t.Fatal(err)
	}
	prefs, err := lc.GetPrefs(ctx)
	if err != nil {
		t.Fatal(err)
	}
	if len(prefs.AdvertiseRoutes) != 0 {
		t.Fatalf("untagged node advertises %v", prefs.AdvertiseRoutes)
	}
}

// testControl runs an offline tailnet: testcontrol and an in-process DERP.
func testControl(t *testing.T) *testcontrol.Server {
	t.Helper()
	netns.SetEnabled(false)
	t.Cleanup(func() { netns.SetEnabled(true) })
	control := &testcontrol.Server{
		DERPMap:        integration.RunDERPAndSTUN(t, logger.Discard, "127.0.0.1"),
		DNSConfig:      &tailcfg.DNSConfig{Proxied: true},
		MagicDNSDomain: "tail-scale.ts.net",
		Logf:           logger.Discard,
		TagOwners:      map[string][]string{"tag:egress": nil, "tag:t3-alice": nil},
	}
	control.HTTPTestServer = httptest.NewServer(control)
	t.Cleanup(control.HTTPTestServer.Close)
	return control
}

func tsnetNode(t *testing.T, control *testcontrol.Server, name string, tags ...string) *tsnet.Server {
	t.Helper()
	dir := filepath.Join(t.TempDir(), name)
	if err := os.MkdirAll(dir, 0o700); err != nil {
		t.Fatal(err)
	}
	s := &tsnet.Server{
		Dir:           dir,
		ControlURL:    control.HTTPTestServer.URL,
		Hostname:      name,
		Store:         new(mem.Store),
		Ephemeral:     true,
		AdvertiseTags: tags,
		UserLogf:      logger.Discard,
	}
	t.Cleanup(func() { s.Close() })
	return s
}

// loopbackEcho serves an echo on a loopback port and returns its address.
func loopbackEcho(t *testing.T, network string) string {
	t.Helper()
	if network == "udp" {
		pc, err := net.ListenPacket("udp", "127.0.0.1:0")
		if err != nil {
			t.Fatal(err)
		}
		t.Cleanup(func() { pc.Close() })
		go func() {
			buf := make([]byte, 1500)
			for {
				n, from, err := pc.ReadFrom(buf)
				if err != nil {
					return
				}
				_, _ = pc.WriteTo(buf[:n], from)
			}
		}()
		return pc.LocalAddr().String()
	}
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { ln.Close() })
	go func() {
		for {
			c, err := ln.Accept()
			if err != nil {
				return
			}
			go func() { _, _ = io.Copy(c, c); c.Close() }()
		}
	}()
	return ln.Addr().String()
}

// tenantTCP sends msg from the tenant, half-closes, and returns what came
// back before the egress closed the connection.
func tenantTCP(t *testing.T, ctx context.Context, from *tsnet.Server, addr, msg string) string {
	t.Helper()
	c, err := from.Dial(ctx, "tcp", addr)
	if err != nil {
		t.Fatal(err)
	}
	defer c.Close()
	_ = c.SetDeadline(time.Now().Add(5 * time.Second))
	_, _ = c.Write([]byte(msg))
	c.(interface{ CloseWrite() error }).CloseWrite()
	var got bytes.Buffer
	_, _ = got.ReadFrom(c)
	return got.String()
}

// tenantUDP sends one datagram from the tenant and returns the reply, or nil
// if none came.
func tenantUDP(t *testing.T, ctx context.Context, from *tsnet.Server, addr string, msg []byte) []byte {
	t.Helper()
	c, err := from.Dial(ctx, "udp", addr)
	if err != nil {
		t.Fatal(err)
	}
	defer c.Close()
	_, _ = c.Write(msg)
	_ = c.SetReadDeadline(time.Now().Add(2 * time.Second))
	buf := make([]byte, 1500)
	n, err := c.Read(buf)
	if err != nil {
		return nil
	}
	return buf[:n]
}
