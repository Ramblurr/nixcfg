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
	"path/filepath"
	"testing"
	"time"

	"tailscale.com/client/local"
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
		{"unix:/run/broker/ui.sock", "error"},
		{"443:3000:1", "error"},
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
}

func headerEchoSocket(t *testing.T) string {
	sock := filepath.Join(t.TempDir(), "upstream.sock")
	ln, err := net.Listen("unix", sock)
	if err != nil {
		t.Fatal(err)
	}
	srv := &http.Server{Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
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
	front := httptest.NewServer(createReverseProxy(upstream))
	defer front.Close()

	req, _ := http.NewRequest(http.MethodGet, front.URL+"/", nil)
	req.Header.Set("X-Probe", "through the socket")
	got := getHeaders(t, http.DefaultClient, req)
	if got.Get("X-Probe") != "through the socket" {
		t.Errorf("upstream saw headers %v, want X-Probe", got)
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

func startControl(t *testing.T) string {
	netns.SetEnabled(false)
	t.Cleanup(func() { netns.SetEnabled(true) })
	control := &testcontrol.Server{
		DERPMap:        startDERP(t),
		DNSConfig:      &tailcfg.DNSConfig{Proxied: true},
		MagicDNSDomain: "tail-scale.ts.net",
		Logf:           logger.Discard,
		TagOwners:      map[string][]string{"tag:ts-broker": nil, "tag:egress": nil},
	}
	control.HTTPTestServer = httptest.NewUnstartedServer(control)
	control.HTTPTestServer.Start()
	t.Cleanup(control.HTTPTestServer.Close)
	return control.HTTPTestServer.URL
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
	controlURL := startControl(t)

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
		if err := startHTTPListener(ctx, plug, lc, "plug", upstream); err != nil {
			t.Error(err)
		}
	}()

	forged := map[string]string{
		"Tailscale-User-Login":       "mallory@example.com",
		"Tailscale-User-Name":        "Mallory",
		"Tailscale-User-Profile-Pic": "https://example.com/mallory.png",
		"Tailscale-Node-Name":        "root-laptop.tail-scale.ts.net",
		"Tailscale-Node-Tags":        "tag:admin",
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
			got := getHeaders(t, caller.HTTPClient(), req)

			for k, v := range forged {
				if vs, ok := got[k]; !ok || len(vs) != 1 || vs[0] == v {
					t.Errorf("%s = %q, want one value set by ts-plug", k, vs)
				}
			}
			if login := got.Get("Tailscale-User-Login"); (login != "") != tt.wantUser {
				t.Errorf("Tailscale-User-Login = %q, want set: %v", login, tt.wantUser)
			}
			if tags := got.Get("Tailscale-Node-Tags"); tags != tt.wantTags {
				t.Errorf("Tailscale-Node-Tags = %q, want %q", tags, tt.wantTags)
			}
			if node, want := got.Get("Tailscale-Node-Name"), tt.host+".tail-scale.ts.net"; node != want {
				t.Errorf("Tailscale-Node-Name = %q, want %q", node, want)
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
