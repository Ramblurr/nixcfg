package main

import (
	"context"
	"crypto/tls"
	"encoding/json"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"net/netip"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"tailscale.com/client/local"
	"tailscale.com/client/tailscale/apitype"
	"tailscale.com/derp/derpserver"
	"tailscale.com/ipn/store/mem"
	"tailscale.com/net/netns"
	"tailscale.com/net/stun/stuntest"
	"tailscale.com/tailcfg"
	"tailscale.com/tsnet"
	"tailscale.com/tstest/integration/testcontrol"
	"tailscale.com/types/key"
	"tailscale.com/types/logger"
	"tailscale.com/types/nettype"
)

func TestPortMapFlag(t *testing.T) {
	for _, tt := range []struct {
		in   string
		want string
	}{
		{"", "443:8080"},
		{"9000", "9000:9000"},
		{"8443:3000", "8443:3000"},
		{"443:unix:/run/broker/ui.sock", "443:unix:/run/broker/ui.sock"},
		{"443:unix:", "error"},
		{"443:unix:@broker-ui", "error"},
		{"443:unix:run/broker/ui.sock", "error"},
		{"unix:/run/broker/ui.sock", "error"},
		{"443:3000:1", "error"},
		{"443:10.77.0.2:3780", "443:10.77.0.2:3780"},
		{"443:[fd00::2]:3780", "443:[fd00::2]:3780"},
		{"443:guest:3780", "error"},
		{"443:10.77.0.2", "error"},
		{"https", "error"},
	} {
		p := NewPortMapFlag(443, 8080)
		got := "error"
		if err := p.Set(tt.in); err == nil {
			got = p.String()
		}
		if got != tt.want {
			t.Errorf("Set(%q) = %s, want %s", tt.in, got, tt.want)
		}
	}

	p := NewPortMapFlag(443, 8080)
	p.Set("443:unix:/run/broker/ui.sock")
	if p.Set("443:3000"); p.String() != "443:3000" {
		t.Errorf("a later -https-port 443:3000 left %s", p)
	}
	p.Set("443:10.77.0.2:3780")
	if p.Set("443:3000"); p.String() != "443:3000" {
		t.Errorf("a later -https-port 443:3000 left %s", p)
	}
}

func TestReverseProxyToAddress(t *testing.T) {
	ln, err := net.Listen("tcp", "127.0.0.2:0")
	if err != nil {
		t.Fatal(err)
	}
	srv := &http.Server{Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		io.WriteString(w, "upstream")
	})}
	go srv.Serve(ln)
	t.Cleanup(func() { srv.Close() })

	upstream := NewPortMapFlag(443, 8080)
	if err := upstream.Set("443:" + ln.Addr().String()); err != nil {
		t.Fatal(err)
	}
	whois := func(context.Context, string) (*apitype.WhoIsResponse, error) {
		return nil, io.EOF
	}
	front := httptest.NewTLSServer(createReverseProxy(upstream, whois))
	defer front.Close()

	resp, err := front.Client().Get(front.URL)
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()
	body, _ := io.ReadAll(resp.Body)
	if string(body) != "upstream" {
		t.Errorf("body = %q, want the upstream at %s", body, ln.Addr())
	}
}

func headerEchoSocket(t *testing.T) string {
	sock := filepath.Join(t.TempDir(), "upstream.sock")
	ln, err := net.Listen("unix", sock)
	if err != nil {
		t.Fatal(err)
	}
	srv := &http.Server{Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		r.Header.Set("Host", r.Host)
		r.Header.Set("X-Raw-Query", r.URL.RawQuery)
		json.NewEncoder(w).Encode(r.Header)
	})}
	go srv.Serve(ln)
	t.Cleanup(func() { srv.Close() })
	return sock
}

func TestReverseProxyToUnixSocket(t *testing.T) {
	upstream := NewPortMapFlag(443, 8080)
	if err := upstream.Set("443:unix:" + headerEchoSocket(t)); err != nil {
		t.Fatal(err)
	}
	whois := func(context.Context, string) (*apitype.WhoIsResponse, error) {
		return &apitype.WhoIsResponse{
			Node:        &tailcfg.Node{Name: "laptop.tail-scale.ts.net."},
			UserProfile: &tailcfg.UserProfile{LoginName: "alice@example.com"},
		}, nil
	}
	front := httptest.NewTLSServer(createReverseProxy(upstream, whois))
	defer front.Close()

	req, _ := http.NewRequest(http.MethodGet, front.URL+"/?item=a;b&session=local", nil)
	req.Header.Set("X-Probe", "through the socket")
	req.Header.Set("Connection", "Tailscale-User-Login, Tailscale-Node-Name, X-Forwarded-Proto")
	got := getHeaders(t, front.Client(), req)
	for k, want := range map[string]string{
		"X-Probe":              "through the socket",
		"X-Raw-Query":          "item=a;b&session=local",
		"Host":                 strings.TrimPrefix(front.URL, "https://"),
		"X-Forwarded-Proto":    "https",
		"Tailscale-User-Login": "alice@example.com",
		"Tailscale-Node-Name":  "laptop.tail-scale.ts.net",
	} {
		if v := got.Get(k); v != want {
			t.Errorf("%s = %q, want %q", k, v, want)
		}
	}
}

func getHeaders(t *testing.T, c *http.Client, req *http.Request) http.Header {
	t.Helper()
	resp, err := c.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()
	body, _ := io.ReadAll(resp.Body)
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status %d: %s", resp.StatusCode, body)
	}
	var h http.Header
	if err := json.Unmarshal(body, &h); err != nil {
		t.Fatalf("decode %q: %v", body, err)
	}
	return h
}

// startDERP runs a DERP and STUN server on loopback, as
// tstest/integration.RunDERPAndSTUN does without that package's
// dependencies.
func startDERP(t *testing.T) *tailcfg.DERPMap {
	d := derpserver.New(key.NewNode(), logger.Discard)
	srv := httptest.NewUnstartedServer(derpserver.Handler(d))
	srv.Config.ErrorLog = logger.StdLogger(logger.Discard)
	srv.Config.TLSNextProto = map[string]func(*http.Server, *tls.Conn, http.Handler){}
	srv.StartTLS()
	stunAddr, stunCleanup := stuntest.ServeWithPacketListener(t, nettype.Std{})
	t.Cleanup(func() {
		srv.CloseClientConnections()
		srv.Close()
		d.Close()
		stunCleanup()
	})
	return &tailcfg.DERPMap{Regions: map[tailcfg.DERPRegionID]*tailcfg.DERPRegion{
		1: {RegionID: 1, RegionCode: "test", Nodes: []*tailcfg.DERPNode{{
			Name:             "t1",
			RegionID:         1,
			HostName:         "127.0.0.1",
			IPv4:             "127.0.0.1",
			IPv6:             "none",
			STUNPort:         stunAddr.Port,
			DERPPort:         srv.Listener.Addr().(*net.TCPAddr).Port,
			InsecureForTests: true,
			STUNTestIP:       "127.0.0.1",
		}}},
	}}
}

// startControl runs a test control server. A non-nil wrap sits in front of
// every request that opens a connection to it.
func startControl(t *testing.T, wrap func(http.Handler) http.Handler) *testcontrol.Server {
	netns.SetEnabled(false)
	t.Cleanup(func() { netns.SetEnabled(true) })
	control := &testcontrol.Server{
		DERPMap:        startDERP(t),
		DNSConfig:      &tailcfg.DNSConfig{Proxied: true},
		MagicDNSDomain: "tail-scale.ts.net",
		Logf:           logger.Discard,
		TagOwners:      map[string][]string{"tag:ts-broker": nil, "tag:egress": nil, "tag:moot-workstead": nil, "tag:moot-outstead": nil},
	}
	var h http.Handler = control
	if wrap != nil {
		h = wrap(h)
	}
	control.HTTPTestServer = httptest.NewUnstartedServer(h)
	control.HTTPTestServer.Start()
	t.Cleanup(control.HTTPTestServer.Close)
	return control
}

func startNode(t *testing.T, ctx context.Context, controlURL, name string, tags ...string) (*tsnet.Server, netip.Addr) {
	dir := filepath.Join(t.TempDir(), name)
	if err := os.MkdirAll(dir, 0o700); err != nil {
		t.Fatal(err)
	}
	s := &tsnet.Server{
		Dir:           dir,
		ControlURL:    controlURL,
		Hostname:      name,
		Store:         new(mem.Store),
		Ephemeral:     true,
		AdvertiseTags: tags,
		Logf:          logger.Discard,
	}
	t.Cleanup(func() { s.Close() })
	st, err := s.Up(ctx)
	if err != nil {
		t.Fatal(err)
	}
	return s, st.TailscaleIPs[0]
}

// The upstream sees the caller's identity as the plug node's WhoIs reports
// it, never as the caller sent it. A tagged caller has no user, so its user
// headers are blank and its tags are set.
func TestIdentityHeaders(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()
	controlURL := startControl(t, nil).HTTPTestServer.URL

	plug, plugIP := startNode(t, ctx, controlURL, "plug")
	lc, err := plug.LocalClient()
	if err != nil {
		t.Fatal(err)
	}
	upstream := NewPortMapFlag(80, 8080)
	if err := upstream.Set("80:unix:" + headerEchoSocket(t)); err != nil {
		t.Fatal(err)
	}
	go func() {
		if err := startHTTPListener(ctx, plug, lc, "plug", upstream); err != nil && ctx.Err() == nil {
			t.Error(err)
		}
	}()

	forged := map[string]string{
		"Tailscale-User-Login":       "mallory@example.com",
		"Tailscale-User-Name":        "Mallory",
		"Tailscale-User-Profile-Pic": "https://example.com/mallory.png",
		"Tailscale-Node-Name":        "root-laptop.tail-scale.ts.net",
		"Tailscale-Node-Tags":        "tag:admin",
		"X-Forwarded-For":            "203.0.113.7",
		"X-Forwarded-Host":           "broker.example.com",
		"X-Forwarded-Proto":          "https",
	}

	for _, tt := range []struct {
		host     string
		tags     []string
		wantUser bool
		wantTags string
	}{
		{"laptop", nil, true, ""},
		{"egress", []string{"tag:egress", "tag:ts-broker"}, false, "tag:egress,tag:ts-broker"},
	} {
		t.Run(tt.host, func(t *testing.T) {
			caller, callerIP := startNode(t, ctx, controlURL, tt.host, tt.tags...)
			waitForPeer(t, ctx, lc, callerIP)

			req, _ := http.NewRequestWithContext(ctx, http.MethodGet, "http://"+plugIP.String()+"/", nil)
			for k, v := range forged {
				req.Header.Set(k, v)
			}
			req.Header.Set("Forwarded", "for=203.0.113.7;host=broker.example.com;proto=https")
			// Naming a header in Connection makes it hop-by-hop, so a proxy
			// that set it before stripping would drop it.
			req.Header.Set("Connection", "Tailscale-Node-Tags, Tailscale-User-Login, X-Forwarded-Proto")
			got := getHeaders(t, caller.HTTPClient(), req)

			for k, v := range forged {
				if vs, ok := got[k]; !ok || len(vs) != 1 || vs[0] == v {
					t.Errorf("%s = %q, want one value set by ts-plug", k, vs)
				}
			}
			if login := got.Get("Tailscale-User-Login"); (login != "") != tt.wantUser {
				t.Errorf("Tailscale-User-Login = %q, want set: %v", login, tt.wantUser)
			}
			for k, want := range map[string]string{
				"Tailscale-Node-Tags": tt.wantTags,
				"Tailscale-Node-Name": tt.host + ".tail-scale.ts.net",
				"X-Forwarded-For":     callerIP.String(),
				"X-Forwarded-Host":    plugIP.String(),
				"X-Forwarded-Proto":   "http",
			} {
				if v := got.Get(k); v != want {
					t.Errorf("%s = %q, want %q", k, v, want)
				}
			}
			if vs, ok := got["Forwarded"]; ok {
				t.Errorf("Forwarded = %q reached the upstream", vs)
			}
		})
	}
}

func waitForPeer(t *testing.T, ctx context.Context, lc *local.Client, ip netip.Addr) {
	t.Helper()
	for {
		if _, err := lc.WhoIs(ctx, ip.String()); err == nil {
			return
		}
		select {
		case <-ctx.Done():
			t.Fatalf("plug never saw %v: %v", ip, ctx.Err())
		case <-time.After(20 * time.Millisecond):
		}
	}
}

func TestCommandOutputInherit(t *testing.T) {
	var stdout, stderr strings.Builder
	cmd := exec.Command("sh", "-c", `echo '{"kind":"use"}'; echo diagnostic >&2`)
	if err := commandOutput(cmd, true, &stdout, &stderr); err != nil {
		t.Fatal(err)
	}
	if err := cmd.Run(); err != nil {
		t.Fatal(err)
	}
	if got, want := stdout.String(), "{\"kind\":\"use\"}\n"; got != want {
		t.Errorf("stdout = %q, want %q", got, want)
	}
	if got, want := stderr.String(), "diagnostic\n"; got != want {
		t.Errorf("stderr = %q, want %q", got, want)
	}
}
