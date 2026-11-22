package egress

import (
	"bufio"
	"bytes"
	"context"
	"crypto/tls"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"maps"
	"net"
	"net/http"
	"net/http/httptest"
	"net/netip"
	"os"
	"slices"
	"strings"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	"golang.org/x/net/dns/dnsmessage"
	"tailscale.com/client/tailscale/apitype"
	"tailscale.com/ipn"
	"tailscale.com/tailcfg"

	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/policy"
)

func TestPeekSNIFromRealClientHello(t *testing.T) {
	for _, v := range []uint16{tls.VersionTLS12, tls.VersionTLS13} {
		client, server := net.Pipe()
		go func() {
			_ = tls.Client(client, &tls.Config{ServerName: "Registry.npmjs.org", MinVersion: v, MaxVersion: v}).Handshake()
			client.Close()
		}()
		pk := peek(server, 2*time.Second)
		if pk.SNI != "Registry.npmjs.org" || pk.Host != "" {
			t.Fatalf("tls %x: sni=%q host=%q", v, pk.SNI, pk.Host)
		}
		if len(pk.Bytes) == 0 || pk.Bytes[0] != 0x16 {
			t.Fatalf("tls %x: replay bytes do not start with a handshake record", v)
		}
		server.Close()
	}
}

func TestPeekReplayIsTheClientHello(t *testing.T) {
	client, server := net.Pipe()
	var sent bytes.Buffer
	go func() {
		rec := &teeConn{Conn: client, w: &sent}
		_ = tls.Client(rec, &tls.Config{ServerName: "github.com"}).Handshake()
		client.Close()
	}()
	pk := peek(server, 2*time.Second)
	server.Close()
	if !bytes.HasPrefix(sent.Bytes(), pk.Bytes) {
		t.Fatal("peeked bytes are not a prefix of what the client sent")
	}
}

type teeConn struct {
	net.Conn
	w *bytes.Buffer
}

func (c *teeConn) Write(p []byte) (int, error) {
	c.w.Write(p)
	return c.Conn.Write(p)
}

func TestPeekHTTPHost(t *testing.T) {
	client, server := net.Pipe()
	req := "GET /x HTTP/1.1\r\nHost: Example.com:80\r\nUser-Agent: t\r\n\r\n"
	go func() { _, _ = client.Write([]byte(req)) }()
	pk := peek(server, 2*time.Second)
	if pk.Host != "Example.com:80" || pk.SNI != "" || string(pk.Bytes) != req {
		t.Fatalf("host=%q sni=%q bytes=%q", pk.Host, pk.SNI, pk.Bytes)
	}
}

func TestPeekServerFirstProtocolTimesOut(t *testing.T) {
	client, server := net.Pipe()
	defer client.Close()
	start := time.Now()
	pk := peek(server, 100*time.Millisecond)
	if pk.SNI != "" || pk.Host != "" || len(pk.Bytes) != 0 {
		t.Fatalf("got %+v from a silent client", pk)
	}
	if time.Since(start) > time.Second {
		t.Fatal("peek did not honour its timeout")
	}
}

func TestPeekNonTLSBinary(t *testing.T) {
	client, server := net.Pipe()
	go func() { _, _ = client.Write([]byte("\x00\x01binary")); client.Close() }()
	pk := peek(server, time.Second)
	if pk.SNI != "" || pk.Host != "" {
		t.Fatalf("named a binary stream: %+v", pk)
	}
}

type fakeWho struct {
	mu    sync.Mutex
	calls int
	res   *apitype.WhoIsResponse
	err   error
}

func (f *fakeWho) WhoIs(context.Context, string) (*apitype.WhoIsResponse, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.calls++
	return f.res, f.err
}

func whoWith(grants ...string) *fakeWho {
	vals := make([]tailcfg.RawMessage, len(grants))
	for i, g := range grants {
		vals[i] = tailcfg.RawMessage(g)
	}
	cm := tailcfg.PeerCapMap{}
	if len(grants) > 0 {
		cm[policy.CapName] = vals
	}
	return &fakeWho{res: &apitype.WhoIsResponse{
		Node:   &tailcfg.Node{ID: 7, ComputedName: "t3-alice", Tags: []string{"tag:t3-alice"}},
		CapMap: cm,
	}}
}

func aliceCaps(t *testing.T) string {
	b, err := os.ReadFile("../policy/testdata/alice.json")
	if err != nil {
		t.Fatal(err)
	}
	return string(b)
}

func TestLookupFailsClosed(t *testing.T) {
	for name, tt := range map[string]struct {
		who  *fakeWho
		want policy.Reason
	}{
		"whois error":   {&fakeWho{err: errors.New("no such peer")}, policy.ReasonNoIdentity},
		"no node":       {&fakeWho{res: &apitype.WhoIsResponse{}}, policy.ReasonNoIdentity},
		"no caps":       {whoWith(), policy.ReasonNoCaps},
		"malformed cap": {whoWith(`{"tls":[{"hosts":["a.com"],"ports":[443],"x":1}]}`), policy.ReasonBadPolicy},
	} {
		t.Run(name, func(t *testing.T) {
			c := NewIdentities(tt.who, time.Minute).Lookup(context.Background(), netip.MustParseAddr("100.100.1.1"))
			if c.Reason != tt.want {
				t.Fatalf("reason = %q, want %q", c.Reason, tt.want)
			}
		})
	}
}

// A tagged node is known by its sorted tags, an untagged one by its
// owner's login.
func TestLookupIdentity(t *testing.T) {
	tagged := whoWith(`{"dns":{"allow":"rules"}}`)
	tagged.res.Node.Tags = []string{"tag:b", "tag:a"}
	tagged.res.UserProfile = &tailcfg.UserProfile{LoginName: "tagged-devices"}
	untagged := whoWith(`{"dns":{"allow":"rules"}}`)
	untagged.res.Node.Tags = nil
	untagged.res.UserProfile = &tailcfg.UserProfile{LoginName: "alice@example.com"}
	for who, want := range map[*fakeWho]string{tagged: "tag:a,tag:b", untagged: "alice@example.com"} {
		c := NewIdentities(who, time.Minute).Lookup(context.Background(), netip.MustParseAddr("100.100.1.1"))
		if got := c.Identity(); got != want {
			t.Errorf("identity = %q, want %q", got, want)
		}
	}
}

func TestLookupCachesUntilTTLOrNetmapChange(t *testing.T) {
	who := whoWith(`{"dns":{"allow":"rules"}}`)
	ids := NewIdentities(who, time.Minute)
	now := time.Unix(1000, 0)
	ids.now = func() time.Time { return now }
	src := netip.MustParseAddr("100.100.1.1")
	ids.Lookup(context.Background(), src)
	ids.Lookup(context.Background(), src)
	if who.calls != 1 {
		t.Fatalf("whois calls = %d, want 1 while cached", who.calls)
	}
	now = now.Add(2 * time.Minute)
	ids.Lookup(context.Background(), src)
	if who.calls != 2 {
		t.Fatalf("whois calls = %d, want 2 after TTL", who.calls)
	}
	ids.Invalidate(&ipn.Notify{PeersRemoved: []tailcfg.NodeID{7}})
	ids.Lookup(context.Background(), src)
	if who.calls != 3 {
		t.Fatalf("whois calls = %d, want 3 after the peer changed", who.calls)
	}
}

// auditBuf collects audit lines as decoded JSON objects.
type auditBuf struct {
	mu  sync.Mutex
	buf bytes.Buffer
}

func (a *auditBuf) Write(p []byte) (int, error) {
	a.mu.Lock()
	defer a.mu.Unlock()
	return a.buf.Write(p)
}

func (a *auditBuf) lines(t *testing.T) []map[string]any {
	t.Helper()
	a.mu.Lock()
	defer a.mu.Unlock()
	var out []map[string]any
	sc := bufio.NewScanner(bytes.NewReader(a.buf.Bytes()))
	for sc.Scan() {
		var m map[string]any
		if err := json.Unmarshal(sc.Bytes(), &m); err != nil {
			t.Fatalf("audit line is not JSON: %q", sc.Text())
		}
		out = append(out, m)
	}
	return out
}

func newServer(who WhoIser, resolver string) (*Server, *auditBuf) {
	ab := &auditBuf{}
	return &Server{
		Floor:       policy.NewFloor(nil),
		IDs:         NewIdentities(who, time.Minute),
		Names:       NewNames(),
		Resolver:    resolver,
		PeekTimeout: 200 * time.Millisecond,
		UDPIdle:     time.Second,
		Audit:       slog.New(slog.NewJSONHandler(ab, nil)),
	}, ab
}

// tcpFlow runs one connection through handleTCP. The listener is on
// loopback, so the original destination is always on the floor.
func tcpFlow(t *testing.T, s *Server, send string) []byte {
	t.Helper()
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	done := make(chan struct{})
	go func() {
		defer close(done)
		c, err := ln.Accept()
		if err == nil {
			s.handleTCP(context.Background(), c)
		}
	}()
	c, err := net.Dial("tcp", ln.Addr().String())
	if err != nil {
		t.Fatal(err)
	}
	defer c.Close()
	_, _ = c.Write([]byte(send))
	_ = c.SetReadDeadline(time.Now().Add(2 * time.Second))
	var got bytes.Buffer
	_, _ = got.ReadFrom(c)
	<-done
	return got.Bytes()
}

func TestTCPDeniedAndAudited(t *testing.T) {
	for name, tt := range map[string]struct {
		who    *fakeWho
		reason policy.Reason
	}{
		"no identity": {&fakeWho{err: errors.New("unknown")}, policy.ReasonNoIdentity},
		"no caps":     {whoWith(), policy.ReasonNoCaps},
		"bad caps":    {whoWith(`{"tcp":[{"ports":[1]}]}`), policy.ReasonBadPolicy},
		"floor":       {whoWith(`{"tcp":[{"cidrs":["0.0.0.0/0"],"ports":[1,65535]}],"tls":[{"hosts":["*"],"ports":[1,65535]}]}`), policy.ReasonFloor},
	} {
		t.Run(name, func(t *testing.T) {
			s, ab := newServer(tt.who, "127.0.0.1:1")
			if got := tcpFlow(t, s, "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n"); len(got) != 0 {
				t.Fatalf("denied flow got %q back", got)
			}
			lines := ab.lines(t)
			if len(lines) != 1 {
				t.Fatalf("audit lines = %d, want 1", len(lines))
			}
			l := lines[0]
			if l["verdict"] != "deny" || l["reason"] != string(tt.reason) || l["proto"] != "tcp" {
				t.Fatalf("audit = %v", l)
			}
			for _, k := range []string{"time", "src", "dst", "bytes_up", "bytes_down", "duration"} {
				if _, ok := l[k]; !ok {
					t.Errorf("audit line lacks %q: %v", k, l)
				}
			}
		})
	}
}

// fakeResolver answers A queries from answers, with no records for other
// names, over UDP and TCP on one port, and counts queries.
func fakeResolver(t *testing.T, answers map[string][4]byte) (string, *atomic.Int32) {
	t.Helper()
	pc, err := net.ListenPacket("udp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { pc.Close() })
	ln, err := net.Listen("tcp", pc.LocalAddr().String())
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { ln.Close() })
	var n atomic.Int32
	respond := func(msg []byte) []byte {
		n.Add(1)
		var p dnsmessage.Parser
		h, _ := p.Start(msg)
		q, _ := p.Question()
		b := dnsmessage.NewBuilder(nil, dnsmessage.Header{ID: h.ID, Response: true})
		_ = b.StartQuestions()
		_ = b.Question(q)
		_ = b.StartAnswers()
		if a, ok := answers[q.Name.String()]; ok && q.Type == dnsmessage.TypeA {
			_ = b.AResource(dnsmessage.ResourceHeader{Name: q.Name, Class: dnsmessage.ClassINET, TTL: 300}, dnsmessage.AResource{A: a})
		}
		resp, _ := b.Finish()
		return resp
	}
	go func() {
		buf := make([]byte, 1500)
		for {
			l, from, err := pc.ReadFrom(buf)
			if err != nil {
				return
			}
			_, _ = pc.WriteTo(respond(buf[:l]), from)
		}
	}()
	go func() {
		for {
			c, err := ln.Accept()
			if err != nil {
				return
			}
			go func() {
				defer c.Close()
				for {
					msg, err := readTCPMsg(c)
					if err != nil || writeTCPMsg(c, respond(msg)) != nil {
						return
					}
				}
			}()
		}
	}()
	return pc.LocalAddr().String(), &n
}

func query(t *testing.T, name string) []byte {
	t.Helper()
	b := dnsmessage.NewBuilder(nil, dnsmessage.Header{ID: 4242, RecursionDesired: true})
	_ = b.StartQuestions()
	_ = b.Question(dnsmessage.Question{Name: dnsmessage.MustNewName(name), Type: dnsmessage.TypeA, Class: dnsmessage.ClassINET})
	q, err := b.Finish()
	if err != nil {
		t.Fatal(err)
	}
	return q
}

func TestDNSFilter(t *testing.T) {
	upstreamIP := [4]byte{140, 82, 112, 3}
	resolver, hits := fakeResolver(t, map[string][4]byte{"github.com.": upstreamIP})
	who := whoWith(aliceCaps(t))
	s, ab := newServer(who, resolver)
	src := netip.MustParseAddrPort("100.100.1.1:5353")
	dst := netip.MustParseAddrPort("100.100.100.100:53")
	caller := s.IDs.Lookup(context.Background(), src.Addr())

	ask := func(name string) dnsmessage.Message {
		rec := &record{start: time.Now(), caller: caller, proto: policy.UDP, src: src, dst: dst}
		resp, err := s.answerDNS(context.Background(), rec, "udp", query(t, name))
		if err != nil {
			t.Fatalf("%s: %v", name, err)
		}
		s.log(rec)
		var m dnsmessage.Message
		if err := m.Unpack(resp); err != nil {
			t.Fatal(err)
		}
		if m.Header.ID != 4242 || !m.Header.Response {
			t.Fatalf("%s: bad response header %+v", name, m.Header)
		}
		return m
	}

	m := ask("github.com.")
	if m.Header.RCode != dnsmessage.RCodeSuccess || len(m.Answers) != 1 {
		t.Fatalf("covered name: rcode=%v answers=%d", m.Header.RCode, len(m.Answers))
	}
	if got := s.Names.For(src.Addr(), netip.AddrFrom4(upstreamIP)); !slices.Equal(got, []string{"github.com"}) {
		t.Fatalf("learned names = %v", got)
	}
	m = ask("example.org.")
	if m.Header.RCode != dnsmessage.RCodeRefused || len(m.Answers) != 0 {
		t.Fatalf("uncovered name: rcode=%v answers=%d", m.Header.RCode, len(m.Answers))
	}
	if hits.Load() != 1 {
		t.Fatalf("upstream saw %d queries, want only the covered one", hits.Load())
	}

	lines := ab.lines(t)
	if len(lines) != 2 || lines[0]["verdict"] != "relay" || lines[0]["question"] != "github.com" ||
		lines[1]["verdict"] != "deny" || lines[1]["reason"] != string(policy.ReasonDNSUncovered) ||
		lines[1]["question"] != "example.org" ||
		lines[0]["node"] != "t3-alice" || !strings.Contains(toJSON(lines[0]["tags"]), "tag:t3-alice") {
		t.Fatalf("audit = %v", lines)
	}
}

// A label may hold a literal dot on the wire. If the question were
// normalized before Decide normalized it again, github.com plus a "com."
// label would pass as github.com. It is never forwarded.
func TestDNSRefusesALiteralDotLabel(t *testing.T) {
	resolver, hits := fakeResolver(t, map[string][4]byte{"github.com.": {140, 82, 112, 3}})
	s, _ := newServer(whoWith(aliceCaps(t)), resolver)
	src := netip.MustParseAddrPort("100.100.1.1:5353")
	rec := &record{start: time.Now(), caller: s.IDs.Lookup(context.Background(), src.Addr()), proto: policy.UDP, src: src, dst: netip.MustParseAddrPort("100.100.100.100:53")}
	q := []byte{0x10, 0x92, 0x01, 0x00, 0, 1, 0, 0, 0, 0, 0, 0}
	q = append(q, 6, 'g', 'i', 't', 'h', 'u', 'b', 4, 'c', 'o', 'm', '.', 0)
	q = append(q, 0, byte(dnsmessage.TypeA), 0, byte(dnsmessage.ClassINET))
	resp, err := s.answerDNS(context.Background(), rec, "udp", q)
	if _, relayed := rec.verdict.(policy.Relay); relayed || hits.Load() != 0 {
		t.Fatalf("verdict %v, upstream saw %d queries", rec.verdict, hits.Load())
	}
	if err == nil {
		var m dnsmessage.Message
		if err := m.Unpack(resp); err != nil || m.Header.RCode != dnsmessage.RCodeRefused {
			t.Fatalf("rcode=%v err=%v", m.Header.RCode, err)
		}
	}
}

func TestDNSRejectsMalformed(t *testing.T) {
	s, _ := newServer(whoWith(aliceCaps(t)), "127.0.0.1:1")
	caller := s.IDs.Lookup(context.Background(), netip.MustParseAddr("100.100.1.1"))
	rec := &record{caller: caller, proto: policy.UDP}
	if resp, err := s.answerDNS(context.Background(), rec, "udp", []byte("junk")); resp != nil || err == nil {
		t.Fatalf("malformed query answered: %v %v", resp, err)
	}
}

func toJSON(v any) string {
	b, _ := json.Marshal(v)
	return string(b)
}

func TestHelper(t *testing.T) {
	ab := &auditBuf{}
	localhost, _ := policy.ParseHostPattern("localhost")
	tsHosts := []policy.HostPattern{localhost}
	for _, h := range DefaultHelperHosts {
		p, err := policy.ParseHostPattern(h)
		if err != nil {
			t.Fatal(err)
		}
		tsHosts = append(tsHosts, p)
	}
	h := &Helper{Hosts: tsHosts, Ports: []uint16{80, 443}, Floor: policy.NewFloor(nil), Audit: slog.New(slog.NewJSONHandler(ab, nil))}
	srv := httptest.NewServer(h)
	defer srv.Close()

	connect := func(target string) int {
		c, err := net.Dial("tcp", srv.Listener.Addr().String())
		if err != nil {
			t.Fatal(err)
		}
		defer c.Close()
		_, _ = c.Write([]byte("CONNECT " + target + " HTTP/1.1\r\nHost: " + target + "\r\n\r\n"))
		res, err := http.ReadResponse(bufio.NewReader(c), nil)
		if err != nil {
			t.Fatal(err)
		}
		return res.StatusCode
	}
	if got := connect("example.com:443"); got != http.StatusForbidden {
		t.Errorf("other host: %d", got)
	}
	if got := connect("controlplane.tailscale.com:22"); got != http.StatusForbidden {
		t.Errorf("other port: %d", got)
	}
	if got := connect("localhost:443"); got != http.StatusBadGateway {
		t.Errorf("allowed name resolving to loopback: %d, want the dial guard to refuse", got)
	}
	res, err := http.Get(srv.URL + "/")
	if err != nil {
		t.Fatal(err)
	}
	if res.StatusCode != http.StatusMethodNotAllowed {
		t.Errorf("GET: %d", res.StatusCode)
	}
	lines := ab.lines(t)
	if len(lines) != 4 || lines[2]["reason"] != string(policy.ReasonFloor) || lines[0]["host"] != "example.com" {
		t.Fatalf("audit = %v", lines)
	}
	for _, p := range []string{"derp1.tailscale.com", "derp12d.tailscale.com", "login.tailscale.com"} {
		if !h.allowed(p) {
			t.Errorf("%s should be allowed", p)
		}
	}
	if h.allowed("tailscale.com.evil.org") || h.allowed("api.tailscale.com") {
		t.Error("helper allows hosts outside the list")
	}
}

// whoSeq answers WhoIs from res in turn, running during on each call while
// it is in flight.
type whoSeq struct {
	mu     sync.Mutex
	res    []*apitype.WhoIsResponse
	calls  int
	during func(call int)
}

func (w *whoSeq) WhoIs(context.Context, string) (*apitype.WhoIsResponse, error) {
	w.mu.Lock()
	i := min(w.calls, len(w.res)-1)
	w.calls++
	w.mu.Unlock()
	if w.during != nil {
		w.during(i)
	}
	return w.res[i], nil
}

// A netmap change that lands while WhoIs is in flight must not let the
// grants WhoIs read before it be cached or returned.
func TestLookupDropsResultInvalidatedInFlight(t *testing.T) {
	stale := whoWith(`{"dns":{"allow":"rules"},"tls":[{"hosts":["a.com"],"ports":[443]}]}`).res
	fresh := whoWith(`{"tls":[{"hosts":["a.com"],"ports":[443]}]}`).res
	who := &whoSeq{res: []*apitype.WhoIsResponse{stale, fresh}}
	ids := NewIdentities(who, time.Minute)
	who.during = func(call int) {
		if call == 0 {
			ids.Invalidate(&ipn.Notify{PeersRemoved: []tailcfg.NodeID{7}})
		}
	}
	src := netip.MustParseAddr("100.100.1.1")
	for i := range 2 {
		if c := ids.Lookup(context.Background(), src); c.Policy.DNS.Allow != policy.DNSNone {
			t.Fatalf("lookup %d returned the grants from before the netmap change", i)
		}
	}
}

// clientHello returns the first TLS record a real client sends for sni.
func clientHello(t *testing.T, sni string) []byte {
	t.Helper()
	client, server := net.Pipe()
	defer server.Close()
	go func() {
		_ = tls.Client(client, &tls.Config{ServerName: sni}).Handshake()
		client.Close()
	}()
	hdr := make([]byte, 5)
	if _, err := io.ReadFull(server, hdr); err != nil {
		t.Fatal(err)
	}
	body := make([]byte, int(hdr[3])<<8|int(hdr[4]))
	if _, err := io.ReadFull(server, body); err != nil {
		t.Fatal(err)
	}
	return append(hdr, body...)
}

// A stream that starts like TLS or HTTP but ends before the hello or the
// request head is complete is incomplete; other protocols are not.
func TestPeekIncompletePreface(t *testing.T) {
	hello := clientHello(t, "bad.test")
	for name, tt := range map[string]struct {
		send       string
		incomplete bool
	}{
		"full hello":          {string(hello), false},
		"truncated hello":     {string(hello[:20]), true},
		"full http head":      {"GET / HTTP/1.1\r\nHost: a.test\r\n\r\n", false},
		"truncated http head": {"GET / HTTP/1.1\r\nHost: a", true},
		"bare method":         {"GE", true},
		"ssh banner":          {"SSH-2.0-OpenSSH_9.6\r\n", false},
		"binary":              {"\x00\x01binary", false},
	} {
		t.Run(name, func(t *testing.T) {
			client, server := net.Pipe()
			defer server.Close()
			go func() { _, _ = io.WriteString(client, tt.send); client.Close() }()
			if pk := peek(server, time.Second); pk.Incomplete != tt.incomplete {
				t.Fatalf("incomplete = %v, want %v (%+v)", pk.Incomplete, tt.incomplete, pk)
			}
		})
	}
}

// Without a Decrypter, a ClientHello that matches an http rule is denied before anything dials upstream. The floor is empty and
// the rule's name resolves to the listener itself, so a pass-through or a
// relay would show up as a second accepted connection.
func TestDecryptWithoutDecrypterDialsNothing(t *testing.T) {
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	port := ln.Addr().(*net.TCPAddr).Port
	resolver, _ := fakeResolver(t, map[string][4]byte{"api.test.": {127, 0, 0, 1}})
	s, ab := newServer(whoWith(fmt.Sprintf(`{"http":[{"hosts":["api.test"],"ports":[%d]}],"tcp":[{"cidrs":["127.0.0.0/8"],"ports":[%d]}]}`, port, port)), resolver)
	s.Floor = policy.Floor{}

	handled := make(chan struct{})
	var upstream atomic.Int32
	go func() {
		c, err := ln.Accept()
		if err != nil {
			return
		}
		go func() { s.handleTCP(context.Background(), c); close(handled) }()
		for {
			c, err := ln.Accept()
			if err != nil {
				return
			}
			upstream.Add(1)
			c.Close()
		}
	}()
	c, err := net.Dial("tcp", ln.Addr().String())
	if err != nil {
		t.Fatal(err)
	}
	_ = c.SetDeadline(time.Now().Add(2 * time.Second))
	_ = tls.Client(c, &tls.Config{ServerName: "api.test"}).Handshake()
	c.Close()
	select {
	case <-handled:
	case <-time.After(3 * time.Second):
		t.Fatal("flow still open after the client closed")
	}
	if n := upstream.Load(); n != 0 {
		t.Fatalf("dialled upstream %d times", n)
	}
	lines := ab.lines(t)
	if len(lines) != 1 || lines[0]["verdict"] != "deny" || lines[0]["reason"] != string(policy.ReasonUndecrypted) || lines[0]["sni"] != "api.test" {
		t.Fatalf("audit = %v", lines)
	}
}

// rawResolver answers each UDP query with respond's bytes.
func rawResolver(t *testing.T, respond func(q dnsmessage.Message) []byte) string {
	t.Helper()
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
			var q dnsmessage.Message
			if q.Unpack(buf[:n]) == nil {
				_, _ = pc.WriteTo(respond(q), from)
			}
		}
	}()
	return pc.LocalAddr().String()
}

type rr struct {
	name, target string
	a            [4]byte
	ttl          uint32
}

// answer builds a response to q with the given CNAME (target set) and A
// records, under question.
func answer(t *testing.T, q dnsmessage.Message, question string, rrs ...rr) []byte {
	t.Helper()
	b := dnsmessage.NewBuilder(nil, dnsmessage.Header{ID: q.ID, Response: true})
	b.EnableCompression()
	_ = b.StartQuestions()
	_ = b.Question(dnsmessage.Question{Name: dnsmessage.MustNewName(question), Type: dnsmessage.TypeA, Class: dnsmessage.ClassINET})
	_ = b.StartAnswers()
	for _, r := range rrs {
		h := dnsmessage.ResourceHeader{Name: dnsmessage.MustNewName(r.name), Class: dnsmessage.ClassINET, TTL: r.ttl}
		var err error
		if r.target != "" {
			err = b.CNAMEResource(h, dnsmessage.CNAMEResource{CNAME: dnsmessage.MustNewName(r.target)})
		} else {
			err = b.AResource(h, dnsmessage.AResource{A: r.a})
		}
		if err != nil {
			t.Fatal(err)
		}
	}
	msg, err := b.Finish()
	if err != nil {
		t.Fatal(err)
	}
	return msg
}

// Learning happens only from a response that validates as a whole against
// the question, and maps addresses to the queried name through its CNAME
// chain, for the shortest TTL along it.
func TestDNSLearnsOnlyFromValidatedChain(t *testing.T) {
	ip := [4]byte{198, 51, 100, 9}
	tests := map[string]struct {
		respond func(t *testing.T, q dnsmessage.Message) []byte
		rcode   dnsmessage.RCode
		learned []string
		ttl     time.Duration
	}{
		"direct answer": {
			respond: func(t *testing.T, q dnsmessage.Message) []byte {
				return answer(t, q, "svc.test.", rr{name: "svc.test.", a: ip, ttl: 300})
			},
			learned: []string{"svc.test"}, ttl: 300 * time.Second,
		},
		"valid answer then malformed one": {
			respond: func(t *testing.T, q dnsmessage.Message) []byte {
				b := answer(t, q, "svc.test.", rr{name: "svc.test.", a: ip, ttl: 300}, rr{name: "svc.test.", a: [4]byte{198, 51, 100, 10}, ttl: 300})
				return b[:len(b)-3]
			},
			rcode: dnsmessage.RCodeServerFailure,
		},
		"question mismatch": {
			respond: func(t *testing.T, q dnsmessage.Message) []byte {
				return answer(t, q, "other.test.", rr{name: "other.test.", a: ip, ttl: 300})
			},
			rcode: dnsmessage.RCodeServerFailure,
		},
		"record for an unrelated name": {
			respond: func(t *testing.T, q dnsmessage.Message) []byte {
				return answer(t, q, "svc.test.", rr{name: "evil.test.", a: ip, ttl: 300})
			},
		},
		"cname chain maps to the queried name": {
			respond: func(t *testing.T, q dnsmessage.Message) []byte {
				return answer(t, q, "svc.test.",
					rr{name: "svc.test.", target: "mid.cdn.test.", ttl: 600},
					rr{name: "mid.cdn.test.", target: "edge.cdn.test.", ttl: 120},
					rr{name: "edge.cdn.test.", a: ip, ttl: 900})
			},
			learned: []string{"svc.test"}, ttl: 120 * time.Second,
		},
		"cname not linked to the query": {
			respond: func(t *testing.T, q dnsmessage.Message) []byte {
				return answer(t, q, "svc.test.",
					rr{name: "evil.test.", target: "edge.cdn.test.", ttl: 600},
					rr{name: "edge.cdn.test.", a: ip, ttl: 900})
			},
		},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			resolver := rawResolver(t, func(q dnsmessage.Message) []byte { return tt.respond(t, q) })
			s, _ := newServer(whoWith(`{"dns":{"allow":"rules"},"tcp":[{"hosts":["svc.test"],"ports":[22]}]}`), resolver)
			now := time.Unix(1000, 0)
			s.Names.now = func() time.Time { return now }
			src := netip.MustParseAddrPort("100.100.1.1:5353")
			caller := s.IDs.Lookup(context.Background(), src.Addr())
			rec := &record{caller: caller, proto: policy.UDP, src: src, dst: netip.MustParseAddrPort("100.100.100.100:53")}
			resp, _ := s.answerDNS(context.Background(), rec, "udp", query(t, "svc.test."))
			var m dnsmessage.Message
			if err := m.Unpack(resp); err != nil {
				t.Fatal(err)
			}
			if m.Header.RCode != tt.rcode {
				t.Fatalf("rcode = %v, want %v", m.Header.RCode, tt.rcode)
			}
			var all []string
			for _, a := range [][4]byte{ip, {198, 51, 100, 10}} {
				all = append(all, s.Names.For(src.Addr(), netip.AddrFrom4(a))...)
			}
			if !slices.Equal(all, tt.learned) {
				t.Fatalf("learned %v, want %v", all, tt.learned)
			}
			if tt.ttl > 0 {
				now = now.Add(tt.ttl - time.Second)
				if len(s.Names.For(src.Addr(), netip.AddrFrom4(ip))) == 0 {
					t.Fatalf("forgot the name before %v", tt.ttl)
				}
				now = now.Add(2 * time.Second)
				if got := s.Names.For(src.Addr(), netip.AddrFrom4(ip)); len(got) != 0 {
					t.Fatalf("still knows %v after %v", got, tt.ttl)
				}
			}
		})
	}
}

// addrConn reports chosen addresses, as a netstack connection does.
type addrConn struct {
	net.Conn
	local, remote net.Addr
}

func (c addrConn) LocalAddr() net.Addr  { return c.local }
func (c addrConn) RemoteAddr() net.Addr { return c.remote }

// Each query in a DNS session is decided under the caller's current
// grants, not the ones the session started with.
func TestDNSSessionReauthorizesEachQuery(t *testing.T) {
	resolver, _ := fakeResolver(t, map[string][4]byte{"svc.test.": {198, 51, 100, 5}})
	who := whoWith(`{"dns":{"allow":"rules"},"tls":[{"hosts":["svc.test"],"ports":[443]}]}`)
	s, ab := newServer(who, resolver)
	client, server := net.Pipe()
	defer client.Close()
	go s.handleTCP(context.Background(), addrConn{
		Conn:   server,
		local:  &net.TCPAddr{IP: net.ParseIP("203.0.113.53"), Port: 53},
		remote: &net.TCPAddr{IP: net.ParseIP("100.100.1.1"), Port: 5000},
	})
	_ = client.SetDeadline(time.Now().Add(3 * time.Second))
	ask := func() dnsmessage.RCode {
		t.Helper()
		if err := writeTCPMsg(client, query(t, "svc.test.")); err != nil {
			t.Fatal(err)
		}
		resp, err := readTCPMsg(client)
		if err != nil {
			t.Fatal(err)
		}
		var m dnsmessage.Message
		if err := m.Unpack(resp); err != nil {
			t.Fatal(err)
		}
		return m.Header.RCode
	}
	if rc := ask(); rc != dnsmessage.RCodeSuccess {
		t.Fatalf("first query rcode = %v", rc)
	}
	who.mu.Lock()
	who.res = whoWith(`{"tls":[{"hosts":["svc.test"],"ports":[443]}]}`).res
	who.mu.Unlock()
	s.IDs.Invalidate(&ipn.Notify{PeersRemoved: []tailcfg.NodeID{7}})
	if rc := ask(); rc != dnsmessage.RCodeRefused {
		t.Fatalf("query after dns was revoked: rcode = %v, want REFUSED", rc)
	}
	// The audit line follows the response.
	var l []map[string]any
	for deadline := time.Now().Add(time.Second); time.Now().Before(deadline); time.Sleep(10 * time.Millisecond) {
		if l = ab.lines(t); len(l) == 2 {
			break
		}
	}
	if len(l) != 2 || l[1]["reason"] != string(policy.ReasonDNSDisabled) {
		t.Fatalf("audit = %v", l)
	}
}

func (s *Server) udpSessionCount() int {
	s.udpMu.Lock()
	defer s.udpMu.Unlock()
	return s.udpSessions
}

// udpSession starts a session from src to dst on a pipe and returns the
// tenant's end and a channel closed when the session ends.
func udpSession(ctx context.Context, s *Server, src, dst string) (net.Conn, <-chan struct{}) {
	client, server := net.Pipe()
	done := make(chan struct{})
	go func() {
		defer close(done)
		s.handleUDP(ctx, netip.MustParseAddrPort(src), netip.MustParseAddrPort(dst), server)
	}()
	return client, done
}

// A denied UDP session ends when the server's context does, not only when
// it idles out.
func TestDeniedUDPSessionEndsOnCancel(t *testing.T) {
	s, _ := newServer(whoWith(), "127.0.0.1:1")
	s.UDPIdle = time.Hour
	ctx, cancel := context.WithCancel(context.Background())
	client, done := udpSession(ctx, s, "100.100.1.1:4000", "198.51.100.5:9")
	defer client.Close()
	for deadline := time.Now().Add(time.Second); s.udpSessionCount() == 0; time.Sleep(10 * time.Millisecond) {
		if time.Now().After(deadline) {
			t.Fatal("session never started")
		}
	}
	cancel()
	select {
	case <-done:
	case <-time.After(time.Second):
		t.Fatal("denied session outlived its context")
	}
	if n := s.udpSessionCount(); n != 0 {
		t.Fatalf("%d sessions still counted", n)
	}
}

// New UDP sessions beyond the per-caller or total bound are closed at once.
func TestUDPSessionLimits(t *testing.T) {
	s, _ := newServer(whoWith(), "127.0.0.1:1")
	s.UDPIdle = time.Hour
	s.MaxUDPSessions, s.MaxUDPSessionsPerCaller = 3, 2
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	live := map[string]int{}
	for _, src := range []string{"100.100.1.1", "100.100.1.1", "100.100.1.1", "100.100.1.2", "100.100.1.2", "100.100.1.3"} {
		for port := 1; port <= 3; port++ {
			client, _ := udpSession(ctx, s, src+":4000", fmt.Sprintf("198.51.100.5:%d", port))
			defer client.Close()
			_ = client.SetReadDeadline(time.Now().Add(100 * time.Millisecond))
			if _, err := client.Read(make([]byte, 1)); !errors.Is(err, io.EOF) {
				live[src]++
			}
		}
	}
	if want := map[string]int{"100.100.1.1": 2, "100.100.1.2": 1}; !maps.Equal(live, want) {
		t.Fatalf("live sessions per caller = %v, want %v", live, want)
	}
}

// Sessions dropped at the bound are logged at the default level, once per
// interval rather than once per flow.
func TestUDPSessionLimitDropsWarnOnce(t *testing.T) {
	var logs auditBuf
	prev := slog.Default()
	slog.SetDefault(slog.New(slog.NewJSONHandler(&logs, nil)))
	t.Cleanup(func() { slog.SetDefault(prev) })
	s, _ := newServer(whoWith(), "127.0.0.1:1")
	s.UDPIdle = time.Hour
	s.MaxUDPSessionsPerCaller = 1
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	held, _ := udpSession(ctx, s, "100.100.1.1:4000", "198.51.100.5:1")
	defer held.Close()
	deadline := time.Now().Add(2 * time.Second)
	for s.liveUDP("100.100.1.1") != 1 {
		if time.Now().After(deadline) {
			t.Fatal("first session never took its slot")
		}
		time.Sleep(time.Millisecond)
	}
	// reject runs a session the bound refuses and waits for its handler,
	// which logs after closing the conn.
	reject := func(port int) {
		client, done := udpSession(ctx, s, "100.100.1.1:4000", fmt.Sprintf("198.51.100.5:%d", port))
		defer client.Close()
		select {
		case <-done:
		case <-time.After(2 * time.Second):
			t.Fatalf("session to port %d was not refused", port)
		}
	}
	for port := 2; port <= 6; port++ {
		reject(port)
	}
	warns := func() []map[string]any {
		var w []map[string]any
		for _, l := range logs.lines(t) {
			if l["level"] == "WARN" {
				w = append(w, l)
			}
		}
		return w
	}
	if w := warns(); len(w) != 1 || w[0]["dropped"] != float64(1) {
		t.Fatalf("warnings = %v, want one for the first drop", w)
	}
	// After the interval the next drop reports every drop since.
	s.udpMu.Lock()
	s.udpWarned = s.udpWarned.Add(-udpDropWarnEvery)
	s.udpMu.Unlock()
	reject(7)
	if w := warns(); len(w) != 2 || w[1]["dropped"] != float64(5) {
		t.Fatalf("warnings = %v, want a second reporting 5 drops", w)
	}
}

// liveUDP is the number of session slots caller holds.
func (s *Server) liveUDP(caller string) int {
	s.udpMu.Lock()
	defer s.udpMu.Unlock()
	return s.perCaller[netip.MustParseAddr(caller)]
}

// closeProbe calls onClose before closing the conn it wraps.
type closeProbe struct {
	net.Conn
	once    sync.Once
	onClose func()
}

func (c *closeProbe) Close() error {
	c.once.Do(c.onClose)
	return c.Conn.Close()
}

// A UDP session closes its conn while it still holds its slot, so a
// datagram after the close opens a new session instead of reaching the
// ending one.
func TestUDPSessionClosesBeforeFreeingItsSlot(t *testing.T) {
	s, _ := newServer(whoWith(), "127.0.0.1:1")
	s.UDPIdle = 50 * time.Millisecond
	client, server := net.Pipe()
	defer client.Close()
	heldAtClose := -1
	probe := &closeProbe{Conn: server, onClose: func() { heldAtClose = s.liveUDP("100.100.1.1") }}
	s.handleUDP(context.Background(), netip.MustParseAddrPort("100.100.1.1:4000"), netip.MustParseAddrPort("198.51.100.5:9"), probe)
	if heldAtClose != 1 {
		t.Fatalf("slots held when the session closed = %d, want 1", heldAtClose)
	}
}

// DNS from a caller the egress cannot identify is denied, and the denied
// exchange gives back its slot as soon as an answered one would.
func TestDeniedDNSFromNewPortsOutlastsTheSessionBound(t *testing.T) {
	s, _ := newServer(whoWith(), "127.0.0.1:1")
	s.UDPIdle = time.Hour
	s.MaxUDPSessionsPerCaller = 1
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	for i := range 3 {
		client, _ := udpSession(ctx, s, fmt.Sprintf("100.100.9.9:%d", 10000+i), "100.100.100.100:53")
		defer client.Close()
		if _, err := client.Write(query(t, "svc.test.")); err != nil {
			t.Fatalf("query %d not taken: %v", i, err)
		}
		time.Sleep(300 * time.Millisecond)
	}
}

// A DNS exchange does not hold its session slot for UDPIdle. Clients
// such as ts-plug send each query from a new port, so a caller that
// resolves more names than the per-caller bound within UDPIdle still gets
// every answer. The tenant never closes its end, as in netstack.
func TestDNSQueriesFromNewPortsOutlastTheSessionBound(t *testing.T) {
	resolver, _ := fakeResolver(t, map[string][4]byte{"svc.test.": {198, 51, 100, 5}})
	s, _ := newServer(whoWith(`{"dns":{"allow":"rules"},"tls":[{"hosts":["svc.test"],"ports":[443]}]}`), resolver)
	s.UDPIdle = time.Minute
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	q := query(t, "svc.test.")
	resp := make([]byte, 512)
	for i := range defaultMaxUDPSessionsPerCaller + 100 {
		client, _ := udpSession(ctx, s, fmt.Sprintf("100.100.1.1:%d", 10000+i), "100.100.100.100:53")
		defer client.Close()
		_ = client.SetDeadline(time.Now().Add(2 * time.Second))
		if _, err := client.Write(q); err != nil {
			t.Fatalf("query %d not taken: %v", i, err)
		}
		n, err := client.Read(resp)
		if err != nil {
			t.Fatalf("query %d not answered: %v", i, err)
		}
		var m dnsmessage.Message
		if err := m.Unpack(resp[:n]); err != nil || len(m.Answers) != 1 {
			t.Fatalf("query %d answer = %+v, %v; want one A record", i, m.Answers, err)
		}
		if a, ok := m.Answers[0].Body.(*dnsmessage.AResource); !ok || a.A != [4]byte{198, 51, 100, 5} {
			t.Fatalf("query %d answered %v, want 198.51.100.5", i, m.Answers[0].Body)
		}
		time.Sleep(time.Millisecond)
	}
}

// When one direction of a splice fails, both connections close, so the
// other direction cannot hang on a silent peer.
func TestSpliceClosesBothEndsOnCopyError(t *testing.T) {
	listen := func() net.Listener {
		ln, err := net.Listen("tcp", "127.0.0.1:0")
		if err != nil {
			t.Fatal(err)
		}
		t.Cleanup(func() { ln.Close() })
		return ln
	}
	upLn, clientLn := listen(), listen()
	go func() {
		c, err := upLn.Accept()
		if err == nil {
			t.Cleanup(func() { c.Close() }) // silent, and never closes first
		}
	}()
	up, err := net.Dial("tcp", upLn.Addr().String())
	if err != nil {
		t.Fatal(err)
	}
	tenant, err := net.Dial("tcp", clientLn.Addr().String())
	if err != nil {
		t.Fatal(err)
	}
	client, err := clientLn.Accept()
	if err != nil {
		t.Fatal(err)
	}
	done := make(chan struct{})
	go func() { splice(client, up); close(done) }()
	_ = tenant.(*net.TCPConn).SetLinger(0)
	tenant.Close() // reset: the client read fails, it does not see EOF
	select {
	case <-done:
	case <-time.After(2 * time.Second):
		t.Fatal("splice still running after the client connection failed")
	}
}

// A decrypted connection re-resolves its caller before each request, so a
// grant revoked while it is open ends it at the next request.
func TestDecryptedFlowEndsWhenTheGrantIsRevoked(t *testing.T) {
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	port := ln.Addr().(*net.TCPAddr).Port
	var upstream atomic.Int32
	up, err := net.Listen("tcp", fmt.Sprintf("127.0.0.2:%d", port))
	if err != nil {
		t.Skip("cannot bind 127.0.0.2:", err)
	}
	go func() {
		_ = http.Serve(up, http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) { upstream.Add(1) }))
	}()
	defer up.Close()
	resolver, _ := fakeResolver(t, map[string][4]byte{"api.test.": {127, 0, 0, 2}})
	who := whoWith(fmt.Sprintf(`{"http":[{"hosts":["api.test"],"ports":[%d]}]}`, port))
	s, ab := newServer(who, resolver)
	s.Floor = policy.Floor{}
	s.Decrypter = &HTTPDecrypter{Secrets: fakeSecrets{}, Audit: s.Audit}
	go func() {
		if c, err := ln.Accept(); err == nil {
			s.handleTCP(context.Background(), c)
		}
	}()
	c, err := net.Dial("tcp", ln.Addr().String())
	if err != nil {
		t.Fatal(err)
	}
	defer c.Close()
	_ = c.SetDeadline(time.Now().Add(5 * time.Second))
	br := bufio.NewReader(c)
	get := func() *http.Response {
		t.Helper()
		_, _ = io.WriteString(c, "GET / HTTP/1.1\r\nHost: api.test\r\n\r\n")
		resp, err := http.ReadResponse(br, nil)
		if err != nil {
			t.Fatal(err)
		}
		_, _ = io.Copy(io.Discard, resp.Body)
		return resp
	}
	if resp := get(); resp.StatusCode != 200 {
		t.Fatalf("first: status %d", resp.StatusCode)
	}
	who.mu.Lock()
	who.res = whoWith().res
	who.mu.Unlock()
	s.IDs.Invalidate(&ipn.Notify{PeersRemoved: []tailcfg.NodeID{7}})
	if resp := get(); resp.StatusCode != http.StatusForbidden || !resp.Close {
		t.Fatalf("revoked: status %d, close %v", resp.StatusCode, resp.Close)
	}
	if _, err := br.ReadByte(); err != io.EOF {
		t.Fatalf("connection still open: %v", err)
	}
	if n := upstream.Load(); n != 1 {
		t.Fatalf("upstream got %d requests", n)
	}
	var reasons []any
	for _, l := range ab.lines(t) {
		if l["msg"] == "request" {
			reasons = append(reasons, l["reason"])
		}
	}
	if len(reasons) != 2 || reasons[0] != nil || reasons[1] != reasonRevoked+":"+string(policy.ReasonNoCaps) {
		t.Fatalf("request reasons = %v", reasons)
	}
}

// A grant update that denies the upstream's address, while the HTTP rule
// and the original destination stay allowed, refuses the next request:
// over a kept-alive upstream connection and over a new dial alike.
func TestDecryptedFlowRedialsUnderTheCurrentPolicy(t *testing.T) {
	for name, keepAlive := range map[string]bool{"reused": true, "new dial": false} {
		t.Run(name, func(t *testing.T) {
			ln, err := net.Listen("tcp", "127.0.0.1:0")
			if err != nil {
				t.Fatal(err)
			}
			defer ln.Close()
			port := ln.Addr().(*net.TCPAddr).Port
			var upstream atomic.Int32
			up, err := net.Listen("tcp", fmt.Sprintf("127.0.0.2:%d", port))
			if err != nil {
				t.Skip("cannot bind 127.0.0.2:", err)
			}
			srv := &http.Server{Handler: http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) { upstream.Add(1) })}
			srv.SetKeepAlivesEnabled(keepAlive)
			go func() { _ = srv.Serve(up) }()
			defer srv.Close()
			resolver, _ := fakeResolver(t, map[string][4]byte{"api.test.": {127, 0, 0, 2}})
			rule := fmt.Sprintf(`{"http":[{"hosts":["api.test"],"ports":[%d]}]}`, port)
			who := whoWith(rule)
			s, ab := newServer(who, resolver)
			s.Floor = policy.Floor{}
			s.Decrypter = &HTTPDecrypter{Secrets: fakeSecrets{}, Audit: s.Audit}
			go func() {
				if c, err := ln.Accept(); err == nil {
					s.handleTCP(context.Background(), c)
				}
			}()
			c, err := net.Dial("tcp", ln.Addr().String())
			if err != nil {
				t.Fatal(err)
			}
			defer c.Close()
			_ = c.SetDeadline(time.Now().Add(5 * time.Second))
			br := bufio.NewReader(c)
			get := func() int {
				t.Helper()
				_, _ = io.WriteString(c, "GET / HTTP/1.1\r\nHost: api.test\r\n\r\n")
				resp, err := http.ReadResponse(br, nil)
				if err != nil {
					t.Fatal(err)
				}
				_, _ = io.Copy(io.Discard, resp.Body)
				return resp.StatusCode
			}
			if code := get(); code != 200 {
				t.Fatalf("first: status %d", code)
			}
			who.mu.Lock()
			who.res = whoWith(rule, `{"deny":{"cidrs":["127.0.0.2/32"]}}`).res
			who.mu.Unlock()
			s.IDs.Invalidate(&ipn.Notify{PeersRemoved: []tailcfg.NodeID{7}})
			if code := get(); code != http.StatusBadGateway {
				t.Fatalf("denied upstream: status %d", code)
			}
			if n := upstream.Load(); n != 1 {
				t.Fatalf("upstream got %d requests", n)
			}
			var reasons []any
			for _, l := range ab.lines(t) {
				if l["msg"] == "request" {
					reasons = append(reasons, l["reason"])
				}
			}
			if len(reasons) != 2 || reasons[0] != nil || reasons[1] != string(policy.ReasonDenyCIDR) {
				t.Fatalf("request reasons = %v", reasons)
			}
		})
	}
}

// queryEDNS is query with an EDNS OPT record, as resolvers that read
// Extended DNS Errors send it.
func queryEDNS(t *testing.T, name string) []byte {
	t.Helper()
	b := dnsmessage.NewBuilder(nil, dnsmessage.Header{ID: 4242, RecursionDesired: true})
	_ = b.StartQuestions()
	_ = b.Question(dnsmessage.Question{Name: dnsmessage.MustNewName(name), Type: dnsmessage.TypeA, Class: dnsmessage.ClassINET})
	_ = b.StartAdditionals()
	var rh dnsmessage.ResourceHeader
	_ = rh.SetEDNS0(1232, dnsmessage.RCodeSuccess, false)
	_ = b.OPTResource(rh, dnsmessage.OPTResource{})
	q, err := b.Finish()
	if err != nil {
		t.Fatal(err)
	}
	return q
}

// edeCodes returns the Extended DNS Error codes in m, and whether m has an
// OPT record at all.
func edeCodes(t *testing.T, m dnsmessage.Message) (codes []uint16, hasOPT bool) {
	t.Helper()
	for _, r := range m.Additionals {
		opt, ok := r.Body.(*dnsmessage.OPTResource)
		if !ok {
			continue
		}
		hasOPT = true
		for _, o := range opt.Options {
			if o.Code == 15 && len(o.Data) >= 2 {
				codes = append(codes, uint16(o.Data[0])<<8|uint16(o.Data[1]))
			}
		}
	}
	return codes, hasOPT
}

// A refusal says why, in the Extended DNS Error a resolver that sent EDNS
// can read: 15 (Blocked) when the caller's policy does not cover the name,
// 18 (Prohibited) when the caller has no policy at all. A query without
// EDNS gets no OPT record back.
func TestDNSRefusalCarriesExtendedError(t *testing.T) {
	resolver, hits := fakeResolver(t, map[string][4]byte{"github.com.": {140, 82, 112, 3}})
	src := netip.MustParseAddrPort("100.100.1.1:5353")
	dst := netip.MustParseAddrPort("100.100.100.100:53")
	ask := func(who WhoIser, q []byte) dnsmessage.Message {
		t.Helper()
		s, _ := newServer(who, resolver)
		rec := &record{start: time.Now(), proto: policy.UDP, src: src, dst: dst}
		resp, err := s.answerDNS(context.Background(), rec, "udp", q)
		if err != nil {
			t.Fatal(err)
		}
		var m dnsmessage.Message
		if err := m.Unpack(resp); err != nil {
			t.Fatal(err)
		}
		return m
	}
	cases := []struct {
		name    string
		who     WhoIser
		q       []byte
		wantOPT bool
		wantEDE []uint16
	}{
		{"uncovered name with EDNS", whoWith(aliceCaps(t)), queryEDNS(t, "example.org."), true, []uint16{15}},
		{"uncovered name without EDNS", whoWith(aliceCaps(t)), query(t, "example.org."), false, nil},
		{"caller without caps", whoWith(), queryEDNS(t, "github.com."), true, []uint16{18}},
	}
	for _, c := range cases {
		m := ask(c.who, c.q)
		codes, hasOPT := edeCodes(t, m)
		if m.Header.RCode != dnsmessage.RCodeRefused || hasOPT != c.wantOPT || !slices.Equal(codes, c.wantEDE) {
			t.Errorf("%s: rcode=%v opt=%v ede=%v, want REFUSED opt=%v ede=%v",
				c.name, m.Header.RCode, hasOPT, codes, c.wantOPT, c.wantEDE)
		}
	}
	if hits.Load() != 0 {
		t.Fatalf("upstream saw %d queries for refused names", hits.Load())
	}
}
