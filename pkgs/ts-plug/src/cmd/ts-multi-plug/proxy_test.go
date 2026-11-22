package main

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"net/netip"
	"net/url"
	"path/filepath"
	"slices"
	"sync"
	"testing"
	"time"

	"golang.org/x/net/dns/dnsmessage"
	"tailscale.com/client/local"
)

// A CONNECT client that half-closes its side must still get the backend's
// reply, which the backend sends only after it reads EOF.
func TestHTTPProxyConnectHalfClose(t *testing.T) {
	backend, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer backend.Close()
	go func() {
		conn, err := backend.Accept()
		if err != nil {
			return
		}
		defer conn.Close()
		request, _ := io.ReadAll(conn)
		fmt.Fprintf(conn, "got:%s", request)
	}()

	proxy := httptest.NewServer(httpProxyHandler((&net.Dialer{}).DialContext))
	defer proxy.Close()

	conn, err := net.Dial("tcp", proxy.Listener.Addr().String())
	if err != nil {
		t.Fatal(err)
	}
	defer conn.Close()
	conn.SetDeadline(time.Now().Add(5 * time.Second))

	target := backend.Addr().String()
	fmt.Fprintf(conn, "CONNECT %s HTTP/1.1\r\nHost: %s\r\n\r\n", target, target)
	reader := bufio.NewReader(conn)
	resp, err := http.ReadResponse(reader, nil)
	if err != nil {
		t.Fatal(err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("CONNECT status = %d, want 200", resp.StatusCode)
	}

	io.WriteString(conn, "hello")
	if err := conn.(*net.TCPConn).CloseWrite(); err != nil {
		t.Fatal(err)
	}
	reply, err := io.ReadAll(reader)
	if err != nil {
		t.Fatal(err)
	}
	if got, want := string(reply), "got:hello"; got != want {
		t.Errorf("reply = %q, want %q", got, want)
	}
}

// With an exit node, the proxy dialer sends every dial to the netstack and
// passes its failure through instead of trying tsnet's Dial, which can fall
// back to the host network. The nil tsnet server would panic if it did.
func TestTailnetDialerViaNetstack(t *testing.T) {
	errNoRoute := errors.New("no route")
	var got []netip.AddrPort
	d := &tailnetDialer{viaNetstack: func(_ context.Context, network string, dst netip.AddrPort) (net.Conn, error) {
		got = append(got, dst)
		return nil, errNoRoute
	}}
	for _, addr := range []string{"192.0.2.1:443", "[2001:db8::1]:80"} {
		if _, err := d.Dial(context.Background(), "tcp", addr); !errors.Is(err, errNoRoute) {
			t.Errorf("Dial(%s) error = %v, want %v", addr, err, errNoRoute)
		}
	}
	want := []netip.AddrPort{netip.MustParseAddrPort("192.0.2.1:443"), netip.MustParseAddrPort("[2001:db8::1]:80")}
	if fmt.Sprint(got) != fmt.Sprint(want) {
		t.Errorf("netstack dials = %v, want %v", got, want)
	}
}

// With an exit node, a name outside the tailnet is resolved by DNS sent
// over the netstack to the configured resolver, so it reaches the egress as
// port 53 traffic, and the dial goes to the address it answers.
func TestTailnetDialerResolvesThroughNetstack(t *testing.T) {
	resolver := netip.MustParseAddrPort("1.1.1.1:53")
	var dials []string
	d := &tailnetDialer{
		lc:          &local.Client{Socket: filepath.Join(t.TempDir(), "none.sock"), UseSocketOnly: true},
		dnsResolver: resolver,
		viaNetstack: func(_ context.Context, network string, dst netip.AddrPort) (net.Conn, error) {
			dials = append(dials, network+" "+dst.String())
			if network == "udp" && dst == resolver {
				client, server := net.Pipe()
				go answerA(t, server, "example.com.", [4]byte{192, 0, 2, 7})
				return client, nil
			}
			return nil, errors.New("no route")
		},
	}
	if _, err := d.Dial(context.Background(), "tcp4", "example.com:443"); err == nil || err.Error() != "no route" {
		t.Fatalf("Dial error = %v, want the netstack's", err)
	}
	if want := []string{"udp 1.1.1.1:53", "tcp4 192.0.2.7:443"}; fmt.Sprint(dials) != fmt.Sprint(want) {
		t.Fatalf("netstack dials = %v, want %v", dials, want)
	}
}

// answerA answers one A query on c for name with ip.
func answerA(t *testing.T, c net.Conn, name string, ip [4]byte) {
	defer c.Close()
	buf := make([]byte, 512)
	n, err := c.Read(buf)
	if err != nil {
		return
	}
	var q dnsmessage.Message
	if err := q.Unpack(buf[:n]); err != nil || len(q.Questions) != 1 || q.Questions[0].Name.String() != name {
		t.Errorf("unexpected query %+v: %v", q, err)
		return
	}
	resp := dnsmessage.Message{
		Header:    dnsmessage.Header{ID: q.ID, Response: true},
		Questions: q.Questions,
		Answers: []dnsmessage.Resource{{
			Header: dnsmessage.ResourceHeader{Name: q.Questions[0].Name, Type: dnsmessage.TypeA, Class: dnsmessage.ClassINET, TTL: 60},
			Body:   &dnsmessage.AResource{A: ip},
		}},
	}
	b, err := resp.Pack()
	if err != nil {
		t.Error(err)
		return
	}
	_, _ = c.Write(b)
}

// A CONNECT through an exit node dials the A answer without waiting for
// the AAAA answer, which here comes only after the dial.
func TestHTTPProxyConnectDialsBeforeAAAAAnswer(t *testing.T) {
	release := make(chan struct{})
	defer close(release)
	dst := connectViaExitNode(t, func(q dnsmessage.Question) []netip.Addr {
		if q.Type == dnsmessage.TypeAAAA {
			<-release
			return []netip.Addr{netip.MustParseAddr("2001:db8::7")}
		}
		return []netip.Addr{netip.MustParseAddr("192.0.2.7")}
	})
	if want := netip.MustParseAddrPort("192.0.2.7:443"); dst != want {
		t.Errorf("CONNECT dialed %s, want %s", dst, want)
	}
}

// A CONNECT through an exit node dials a host that has only an AAAA answer.
func TestHTTPProxyConnectDialsAAAAOnlyHost(t *testing.T) {
	dst := connectViaExitNode(t, func(q dnsmessage.Question) []netip.Addr {
		if q.Type == dnsmessage.TypeAAAA {
			return []netip.Addr{netip.MustParseAddr("2001:db8::7")}
		}
		return nil
	})
	if want := netip.MustParseAddrPort("[2001:db8::7]:443"); dst != want {
		t.Errorf("CONNECT dialed %s, want %s", dst, want)
	}
}

// connectViaExitNode sends CONNECT example.com:443 to a proxy whose dialer
// goes through an exit node, with DNS answered by answer, and returns the
// address the proxy dialed. It fails the test if no dial comes within 2 s
// of the first DNS query, which bounds the wait without counting the
// LocalAPI status call before it.
func connectViaExitNode(t *testing.T, answer func(dnsmessage.Question) []netip.Addr) netip.AddrPort {
	t.Helper()
	resolver := netip.MustParseAddrPort("1.1.1.1:53")
	dialed := make(chan netip.AddrPort, 1)
	queried := make(chan struct{})
	var queriedOnce sync.Once
	d := &tailnetDialer{
		lc:          &local.Client{Socket: filepath.Join(t.TempDir(), "none.sock"), UseSocketOnly: true},
		dnsResolver: resolver,
		viaNetstack: func(_ context.Context, network string, dst netip.AddrPort) (net.Conn, error) {
			if network == "udp" && dst == resolver {
				queriedOnce.Do(func() { close(queried) })
				client, server := net.Pipe()
				go serveDNS(t, server, answer)
				return client, nil
			}
			dialed <- dst
			return nil, errors.New("no route")
		},
	}
	proxy := httptest.NewServer(httpProxyHandler(d.Dial))
	t.Cleanup(proxy.Close)
	conn, err := net.Dial("tcp", proxy.Listener.Addr().String())
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { conn.Close() })
	fmt.Fprintf(conn, "CONNECT example.com:443 HTTP/1.1\r\nHost: example.com:443\r\n\r\n")
	select {
	case <-queried:
	case <-time.After(10 * time.Second):
		t.Fatal("CONNECT sent no DNS query within 10 s")
	}
	select {
	case dst := <-dialed:
		return dst
	case <-time.After(2 * time.Second):
		t.Fatal("CONNECT did not dial within 2 s of its first DNS query")
		return netip.AddrPort{}
	}
}

// serveDNS answers one query on c with the addresses answer gives for its
// question, keeping those of the asked type.
func serveDNS(t *testing.T, c net.Conn, answer func(dnsmessage.Question) []netip.Addr) {
	defer c.Close()
	buf := make([]byte, 512)
	n, err := c.Read(buf)
	if err != nil {
		return
	}
	var q dnsmessage.Message
	if err := q.Unpack(buf[:n]); err != nil || len(q.Questions) != 1 {
		t.Errorf("unexpected query %+v: %v", q, err)
		return
	}
	question := q.Questions[0]
	resp := dnsmessage.Message{
		Header:    dnsmessage.Header{ID: q.ID, Response: true},
		Questions: q.Questions,
	}
	for _, ip := range answer(question) {
		h := dnsmessage.ResourceHeader{Name: question.Name, Class: dnsmessage.ClassINET, TTL: 60}
		switch {
		case question.Type == dnsmessage.TypeA && ip.Is4():
			h.Type = dnsmessage.TypeA
			resp.Answers = append(resp.Answers, dnsmessage.Resource{Header: h, Body: &dnsmessage.AResource{A: ip.As4()}})
		case question.Type == dnsmessage.TypeAAAA && ip.Is6():
			h.Type = dnsmessage.TypeAAAA
			resp.Answers = append(resp.Answers, dnsmessage.Resource{Header: h, Body: &dnsmessage.AAAAResource{AAAA: ip.As16()}})
		}
	}
	b, err := resp.Pack()
	if err != nil {
		t.Error(err)
		return
	}
	_, _ = c.Write(b)
}

func TestParseDNSResolver(t *testing.T) {
	for _, tt := range []struct {
		exitNode, resolver string
		want               string
	}{
		{"", "", "invalid AddrPort"},
		{"egress", "1.1.1.1:53", "1.1.1.1:53"},
		{"egress", "[2606:4700:4700::1111]:53", "[2606:4700:4700::1111]:53"},
		{"egress", "", "error"},
		{"egress", "dns.google:53", "error"},
		{"egress", "1.1.1.1", "error"},
		{"egress", "1.1.1.1:0", "error"},
		{"", "1.1.1.1:53", "error"},
	} {
		ap, err := parseDNSResolver(tt.exitNode, tt.resolver)
		got := ap.String()
		if err != nil {
			got = "error"
		}
		if got != tt.want {
			t.Errorf("parseDNSResolver(%q, %q) = %s, want %s", tt.exitNode, tt.resolver, got, tt.want)
		}
	}
}

// answerRefused answers one query on c with rcode and, when the query
// carried EDNS and infoCode is set, an Extended DNS Error. It records
// whether the query carried EDNS.
func answerRefused(t *testing.T, c net.Conn, rcode dnsmessage.RCode, infoCode int, sawEDNS *bool) {
	defer c.Close()
	buf := make([]byte, 512)
	n, err := c.Read(buf)
	if err != nil {
		return
	}
	var q dnsmessage.Message
	if err := q.Unpack(buf[:n]); err != nil {
		t.Errorf("unexpected query: %v", err)
		return
	}
	resp := dnsmessage.Message{
		Header:    dnsmessage.Header{ID: q.ID, Response: true, RCode: rcode},
		Questions: q.Questions,
	}
	for _, r := range q.Additionals {
		if r.Header.Type != dnsmessage.TypeOPT {
			continue
		}
		*sawEDNS = true
		var rh dnsmessage.ResourceHeader
		_ = rh.SetEDNS0(1232, rcode, false)
		var opt dnsmessage.OPTResource
		if infoCode >= 0 {
			opt.Options = []dnsmessage.Option{{Code: 15, Data: []byte{byte(infoCode >> 8), byte(infoCode)}}}
		}
		resp.Additionals = []dnsmessage.Resource{{Header: rh, Body: &opt}}
	}
	b, err := resp.Pack()
	if err != nil {
		t.Error(err)
		return
	}
	_, _ = c.Write(b)
}

// A name the egress refuses reaches the proxy client as the resolver's
// answer, not as a generic dial failure, so the proxy can report it.
func TestTailnetDialerReportsRefusedName(t *testing.T) {
	resolver := netip.MustParseAddrPort("1.1.1.1:53")
	var sawEDNS bool
	d := &tailnetDialer{
		lc:          &local.Client{Socket: filepath.Join(t.TempDir(), "none.sock"), UseSocketOnly: true},
		dnsResolver: resolver,
		viaNetstack: func(_ context.Context, network string, dst netip.AddrPort) (net.Conn, error) {
			if network == "udp" && dst == resolver {
				client, server := net.Pipe()
				go answerRefused(t, server, dnsmessage.RCodeRefused, 15, &sawEDNS)
				return client, nil
			}
			t.Errorf("dialed %s %s after a refused lookup", network, dst)
			return nil, errors.New("no route")
		},
	}
	_, err := d.Dial(context.Background(), "tcp4", "google.com:443")
	var de *dnsError
	if !errors.As(err, &de) || de.rcode != dnsmessage.RCodeRefused || de.infoCode != 15 {
		t.Fatalf("Dial error = %v, want a REFUSED answer with Extended DNS Error 15", err)
	}
	if !sawEDNS {
		t.Errorf("the resolver query carried no EDNS, so no Extended DNS Error can come back")
	}
}

// The proxy answers 403 when the egress refused the name by policy, 502
// for any other failure, and says which in a Proxy-Status header (RFC 9209).
func TestHTTPProxyReportsDNSFailures(t *testing.T) {
	cases := []struct {
		name       string
		err        error
		wantStatus int
		wantProxy  string
	}{
		{"blocked by policy", &dnsError{rcode: dnsmessage.RCodeRefused, infoCode: 15}, http.StatusForbidden,
			`ts-plug; error=dns_error; rcode="REFUSED"; info-code=15`},
		{"caller prohibited", &dnsError{rcode: dnsmessage.RCodeRefused, infoCode: 18}, http.StatusForbidden,
			`ts-plug; error=dns_error; rcode="REFUSED"; info-code=18`},
		{"refused without a reason", &dnsError{rcode: dnsmessage.RCodeRefused, infoCode: -1}, http.StatusBadGateway,
			`ts-plug; error=dns_error; rcode="REFUSED"`},
		{"no such name", &dnsError{rcode: dnsmessage.RCodeNameError, infoCode: -1}, http.StatusBadGateway,
			`ts-plug; error=dns_error; rcode="NXDOMAIN"`},
		{"other dial failure", errors.New("no route"), http.StatusBadGateway, ""},
	}
	for _, c := range cases {
		dial := func(context.Context, string, string) (net.Conn, error) {
			return nil, fmt.Errorf("A query: %w", c.err)
		}
		proxy := httptest.NewServer(httpProxyHandler(dial))
		for _, method := range []string{http.MethodConnect, http.MethodGet} {
			conn, err := net.Dial("tcp", proxy.Listener.Addr().String())
			if err != nil {
				t.Fatal(err)
			}
			conn.SetDeadline(time.Now().Add(5 * time.Second))
			if method == http.MethodConnect {
				fmt.Fprintf(conn, "CONNECT google.com:443 HTTP/1.1\r\nHost: google.com:443\r\n\r\n")
			} else {
				fmt.Fprintf(conn, "GET http://google.com/ HTTP/1.1\r\nHost: google.com\r\n\r\n")
			}
			resp, err := http.ReadResponse(bufio.NewReader(conn), nil)
			conn.Close()
			if err != nil {
				t.Fatal(err)
			}
			if resp.StatusCode != c.wantStatus || resp.Header.Get("Proxy-Status") != c.wantProxy {
				t.Errorf("%s %s: status %d, Proxy-Status %q; want %d, %q", c.name, method,
					resp.StatusCode, resp.Header.Get("Proxy-Status"), c.wantStatus, c.wantProxy)
			}
		}
		proxy.Close()
	}
}

func TestHTTPProxyOnUnixSocket(t *testing.T) {
	backend := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "%s %s", r.Host, r.URL.RequestURI())
	}))
	defer backend.Close()

	socket := filepath.Join(t.TempDir(), "proxy.sock")
	var dialed []string
	dial := func(ctx context.Context, network, addr string) (net.Conn, error) {
		dialed = append(dialed, addr)
		return (&net.Dialer{}).DialContext(ctx, network, backend.Listener.Addr().String())
	}
	failed := make(chan error, 1)
	if err := startLocalProxies(dial, "", "unix:"+socket, func(err error) { failed <- err }); err != nil {
		t.Fatal(err)
	}

	client := &http.Client{Transport: &http.Transport{
		Proxy: http.ProxyURL(&url.URL{Scheme: "http", Host: "proxy"}),
		DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
			return (&net.Dialer{}).DialContext(ctx, "unix", socket)
		},
	}}
	resp, err := client.Get("http://op-connect.tail-scale.ts.net:8080/v1/vaults?x=1")
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()
	body, _ := io.ReadAll(resp.Body)
	if got, want := string(body), "op-connect.tail-scale.ts.net:8080 /v1/vaults?x=1"; got != want {
		t.Errorf("body = %q, want %q", got, want)
	}
	if want := []string{"op-connect.tail-scale.ts.net:8080"}; !slices.Equal(dialed, want) {
		t.Errorf("dialed %q, want %q", dialed, want)
	}
	select {
	case err := <-failed:
		t.Fatal(err)
	default:
	}
}

func TestListenLocalRefusesRelativeAndAbstractSockets(t *testing.T) {
	for _, addr := range []string{"unix:proxy.sock", "unix:@proxy"} {
		if l, err := listenLocal(addr); err == nil {
			l.Close()
			t.Errorf("listenLocal(%q) succeeded, want an error", addr)
		}
	}
}

func TestListenLocalReplacesAStaleSocket(t *testing.T) {
	addr := "unix:" + filepath.Join(t.TempDir(), "proxy.sock")
	stale, err := listenLocal(addr)
	if err != nil {
		t.Fatal(err)
	}
	stale.(*net.UnixListener).SetUnlinkOnClose(false)
	stale.Close()
	l, err := listenLocal(addr)
	if err != nil {
		t.Fatalf("listen over a stale socket: %v", err)
	}
	l.Close()
}
