package egress

import (
	"bytes"
	"context"
	"crypto/tls"
	"crypto/x509"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"net/netip"
	"os"
	"syscall"
	"testing"
	"time"

	"golang.org/x/net/dns/dnsmessage"
	"golang.org/x/sys/unix"
)

// TestTPROXYEndToEnd drives real TPROXY'd flows through the server. It needs
// the throwaway network namespace that scripts/tproxy-e2e.sh builds: tenant
// address 100.100.1.1, upstream address 198.51.100.5, and nftables sending
// every packet from the tenant address to port 15001.
func TestTPROXYEndToEnd(t *testing.T) {
	if os.Getenv("TS_EGRESS_E2E_NETNS") != "1" {
		t.Skip("run through scripts/tproxy-e2e.sh")
	}
	const tenant, upstream = "100.100.1.1", "198.51.100.5"
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	resolver, _ := fakeResolver(t, map[string][4]byte{
		"svc.test.":    {198, 51, 100, 5},
		"ssh.test.":    {198, 51, 100, 5},
		"loop.test.":   {127, 0, 0, 1},
		"example.com.": {198, 51, 100, 5},
	})
	who := whoWith(`{
		"tcp": [{"cidrs": ["198.51.100.5/32"], "ports": [7000]}, {"hosts": ["ssh.test"], "ports": [7003]}],
		"udp": [{"cidrs": ["198.51.100.5/32"], "ports": [7002]}],
		"tls": [{"hosts": ["svc.test", "loop.test"], "ports": [8443]}],
		"http": [{"hosts": ["example.com"], "ports": [8443], "credentials": [
			{"name": "DUMMY_TOKEN", "secret": "literal:egress-dummy-value", "inject": {"header": "X-Dummy-Token", "format": "{secret}"}}
		]}],
		"deny": {"hosts": ["bad.test"]},
		"dns": {"allow": "rules"}
	}`)
	s, ab := newServer(who, resolver)
	const identityTTL = 300 * time.Millisecond
	s.IDs = NewIdentities(who, identityTTL)
	ln, err := ListenTCP(ctx, "tcp4", "0.0.0.0:15001")
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	pc, err := ListenUDP(ctx, "udp4", "0.0.0.0:15001")
	if err != nil {
		t.Fatal(err)
	}
	defer pc.Close()
	go s.ServeTCP(ctx, ln)
	go s.ServeUDP(ctx, pc)

	echoTCP(t, upstream+":7000")
	echoTCP(t, upstream+":7001")
	echoTCP(t, upstream+":7003")
	echoUDP(t, upstream+":7002")
	tlsSrv := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		note := ""
		if r.Header.Get("X-Dummy-Token") == "egress-dummy-value" {
			note = " with the token"
		}
		_, _ = io.WriteString(w, "hello from "+r.TLS.ServerName+note)
	}))
	tlsSrv.Listener, err = net.Listen("tcp", upstream+":8443")
	if err != nil {
		t.Fatal(err)
	}
	tlsSrv.StartTLS()
	defer tlsSrv.Close()
	leaves, _ := testCA(t)
	roots := x509.NewCertPool()
	roots.AddCert(tlsSrv.Certificate())
	s.Decrypter = &HTTPDecrypter{Leaves: leaves, Secrets: fakeSecrets{}, RootCAs: roots, Audit: s.Audit}

	dialer := &net.Dialer{LocalAddr: &net.TCPAddr{IP: net.ParseIP(tenant)}, Timeout: 3 * time.Second}
	udpFrom := &net.UDPAddr{IP: net.ParseIP(tenant)}

	t.Run("tcp relay by cidr", func(t *testing.T) {
		if got := tcpRoundTrip(t, dialer, upstream+":7000", "ping"); got != "ping" {
			t.Fatalf("echo = %q", got)
		}
	})
	t.Run("denied sni is not relayed by cidr", func(t *testing.T) {
		if got := tcpRoundTrip(t, dialer, upstream+":7000", string(clientHello(t, "bad.test"))); got != "" {
			t.Fatalf("denied sni echoed %d bytes", len(got))
		}
	})
	t.Run("incomplete preface is not relayed by cidr", func(t *testing.T) {
		for _, send := range []string{string(clientHello(t, "bad.test")[:20]), "GET / HTTP/1.1\r\nHost: bad"} {
			if got := tcpRoundTrip(t, dialer, upstream+":7000", send); got != "" {
				t.Fatalf("incomplete preface echoed %q", got)
			}
		}
	})
	t.Run("tcp port without rule", func(t *testing.T) {
		if got := tcpRoundTrip(t, dialer, upstream+":7001", "ping"); got != "" {
			t.Fatalf("denied flow echoed %q", got)
		}
	})
	t.Run("tls passthrough dials by name", func(t *testing.T) {
		// The tenant aims at an unrelated address; the egress must dial svc.test.
		body, err := httpsGet(dialer, "203.0.113.9:8443", "svc.test")
		if err != nil || body != "hello from svc.test" {
			t.Fatalf("body=%q err=%v", body, err)
		}
	})
	t.Run("http rule decrypts and injects", func(t *testing.T) {
		body, err := httpsGet(dialer, "203.0.113.9:8443", "example.com")
		if err != nil || body != "hello from example.com with the token" {
			t.Fatalf("body=%q err=%v", body, err)
		}
	})
	t.Run("tls sni without rule", func(t *testing.T) {
		if body, err := httpsGet(dialer, upstream+":8443", "other.test"); err == nil {
			t.Fatalf("denied sni got %q", body)
		}
	})
	t.Run("tls name resolving to the floor", func(t *testing.T) {
		if body, err := httpsGet(dialer, upstream+":8443", "loop.test"); err == nil {
			t.Fatalf("floored dial got %q", body)
		}
	})
	t.Run("udp relay replies from the original destination", func(t *testing.T) {
		c, err := net.DialUDP("udp4", udpFrom, net.UDPAddrFromAddrPort(netip.MustParseAddrPort(upstream+":7002")))
		if err != nil {
			t.Fatal(err)
		}
		defer c.Close()
		_, _ = c.Write([]byte("ping"))
		_ = c.SetReadDeadline(time.Now().Add(3 * time.Second))
		buf := make([]byte, 64)
		n, err := c.Read(buf)
		if err != nil || string(buf[:n]) != "ping" {
			t.Fatalf("udp echo = %q, %v", buf[:n], err)
		}
	})
	t.Run("dns to any resolver address is filtered", func(t *testing.T) {
		if rc := dnsAsk(t, udpFrom, "svc.test."); rc != dnsmessage.RCodeSuccess {
			t.Fatalf("covered name rcode = %v", rc)
		}
		if rc := dnsAsk(t, udpFrom, "other.test."); rc != dnsmessage.RCodeRefused {
			t.Fatalf("uncovered name rcode = %v", rc)
		}
	})
	t.Run("tcp host rule needs the name resolved through egress", func(t *testing.T) {
		if got := tcpRoundTrip(t, dialer, upstream+":7003", "ping"); got != "" {
			t.Fatalf("unresolved name echoed %q", got)
		}
		dnsAsk(t, udpFrom, "ssh.test.")
		if got := tcpRoundTrip(t, dialer, upstream+":7003", "ping"); got != "ping" {
			t.Fatalf("echo after resolving = %q", got)
		}
	})

	t.Run("revoked grants end a live udp relay and dns session", func(t *testing.T) {
		relay, err := net.DialUDP("udp4", udpFrom, net.UDPAddrFromAddrPort(netip.MustParseAddrPort(upstream+":7002")))
		if err != nil {
			t.Fatal(err)
		}
		defer relay.Close()
		dns, err := net.DialUDP("udp4", udpFrom, &net.UDPAddr{IP: net.ParseIP("203.0.113.53"), Port: 53})
		if err != nil {
			t.Fatal(err)
		}
		defer dns.Close()
		echo := func() string {
			_, _ = relay.Write([]byte("ping"))
			_ = relay.SetReadDeadline(time.Now().Add(time.Second))
			buf := make([]byte, 64)
			n, _ := relay.Read(buf)
			return string(buf[:n])
		}
		if got := echo(); got != "ping" {
			t.Fatalf("udp echo before revocation = %q", got)
		}
		if rc := dnsAskOn(t, dns, "svc.test."); rc != dnsmessage.RCodeSuccess {
			t.Fatalf("dns before revocation rcode = %v", rc)
		}
		who.mu.Lock()
		saved := who.res
		who.res = whoWith(`{"tls":[{"hosts":["svc.test"],"ports":[8443]}]}`).res
		who.mu.Unlock()
		defer func() { who.mu.Lock(); who.res = saved; who.mu.Unlock() }()
		time.Sleep(3 * identityTTL)
		if got := echo(); got != "" {
			t.Fatalf("revoked udp relay still echoed %q", got)
		}
		if rc := dnsAskOn(t, dns, "svc.test."); rc != dnsmessage.RCodeRefused {
			t.Fatalf("dns after revocation rcode = %v, want REFUSED", rc)
		}
	})

	// UDP sessions log when they end, which cancel triggers asynchronously.
	cancel()
	var verdicts map[string]int
	for deadline := time.Now().Add(2 * time.Second); time.Now().Before(deadline); time.Sleep(20 * time.Millisecond) {
		verdicts = map[string]int{}
		for _, l := range ab.lines(t) {
			if l["msg"] == "flow" {
				verdicts[l["verdict"].(string)]++
			}
		}
		if verdicts["relay"] >= 5 && verdicts["passthrough"] >= 1 && verdicts["decrypt"] >= 1 && verdicts["deny"] >= 5 {
			return
		}
	}
	t.Fatalf("audit verdict counts = %v", verdicts)
}

func echoTCP(t *testing.T, addr string) {
	ln, err := net.Listen("tcp", addr)
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
}

// echoUDP shares its address with the egress reply socket, which also binds
// the original destination, so it needs SO_REUSEADDR like that socket.
func echoUDP(t *testing.T, addr string) {
	lc := net.ListenConfig{Control: func(_, _ string, c syscall.RawConn) error {
		return c.Control(func(fd uintptr) { _ = unix.SetsockoptInt(int(fd), unix.SOL_SOCKET, unix.SO_REUSEADDR, 1) })
	}}
	pc, err := lc.ListenPacket(context.Background(), "udp", addr)
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
}

func tcpRoundTrip(t *testing.T, d *net.Dialer, addr, msg string) string {
	c, err := d.Dial("tcp", addr)
	if err != nil {
		t.Fatal(err)
	}
	defer c.Close()
	_, _ = c.Write([]byte(msg))
	c.(*net.TCPConn).CloseWrite()
	_ = c.SetReadDeadline(time.Now().Add(3 * time.Second))
	var got bytes.Buffer
	_, _ = got.ReadFrom(c)
	return got.String()
}

func httpsGet(d *net.Dialer, addr, sni string) (string, error) {
	tr := &http.Transport{
		DialContext:     func(ctx context.Context, _, _ string) (net.Conn, error) { return d.DialContext(ctx, "tcp", addr) },
		TLSClientConfig: &tls.Config{ServerName: sni, InsecureSkipVerify: true},
	}
	defer tr.CloseIdleConnections()
	res, err := (&http.Client{Transport: tr, Timeout: 3 * time.Second}).Get("https://" + sni + "/")
	if err != nil {
		return "", err
	}
	defer res.Body.Close()
	b, err := io.ReadAll(res.Body)
	return string(b), err
}

func dnsAsk(t *testing.T, from *net.UDPAddr, name string) dnsmessage.RCode {
	c, err := net.DialUDP("udp4", from, &net.UDPAddr{IP: net.ParseIP("203.0.113.53"), Port: 53})
	if err != nil {
		t.Fatal(err)
	}
	defer c.Close()
	return dnsAskOn(t, c, name)
}

// dnsAskOn sends one query on c, so repeated calls share a DNS session.
func dnsAskOn(t *testing.T, c *net.UDPConn, name string) dnsmessage.RCode {
	t.Helper()
	_, _ = c.Write(query(t, name))
	_ = c.SetReadDeadline(time.Now().Add(3 * time.Second))
	buf := make([]byte, 1500)
	n, err := c.Read(buf)
	if err != nil {
		t.Fatalf("%s: %v", name, err)
	}
	var m dnsmessage.Message
	if err := m.Unpack(buf[:n]); err != nil {
		t.Fatal(err)
	}
	return m.Header.RCode
}
