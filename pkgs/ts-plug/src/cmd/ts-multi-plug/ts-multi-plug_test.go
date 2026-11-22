package main

import (
	"encoding/json"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"path/filepath"
	"testing"
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
