package egress

import (
	"bufio"
	"bytes"
	"compress/gzip"
	"context"
	"crypto/ecdsa"
	"crypto/elliptic"
	"crypto/rand"
	"crypto/tls"
	"crypto/x509"
	"crypto/x509/pkix"
	"encoding/base64"
	"encoding/json"
	"encoding/pem"
	"errors"
	"io"
	"log/slog"
	"math/big"
	"net"
	"net/http"
	"net/http/httptest"
	"net/netip"
	"slices"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/policy"
)

const (
	dummySecret = "egress-dummy-value"
	ghSecret    = "ghp_real-github-token"
)

var (
	dummyCred = policy.Credential{
		Name:   "DUMMY_TOKEN",
		Secret: policy.SecretRef{Scheme: policy.SecretLiteral, Ref: dummySecret},
		Inject: policy.Inject{Header: "X-Dummy-Token", Format: "{secret}"},
	}
	ghCred = policy.Credential{
		Name:   "GITHUB_TOKEN",
		Secret: policy.SecretRef{Scheme: policy.SecretOnePassword, Ref: "op://LLM/github-test/credential"},
		Inject: policy.Inject{Header: "Authorization", Format: "Bearer {secret}"},
	}
)

type fakeSecrets struct{ err error }

func (f fakeSecrets) Get(_ context.Context, ref policy.SecretRef) (string, error) {
	if f.err != nil {
		return "", f.err
	}
	switch ref.Scheme {
	case policy.SecretLiteral:
		return ref.Ref, nil
	case policy.SecretOnePassword:
		return ghSecret, nil
	}
	return "", errors.New("unknown scheme")
}

func testCA(t *testing.T) (*Leaves, *x509.CertPool) {
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
	kder, err := x509.MarshalECPrivateKey(key)
	if err != nil {
		t.Fatal(err)
	}
	leaves, err := NewLeaves(
		pem.EncodeToMemory(&pem.Block{Type: "CERTIFICATE", Bytes: der}),
		pem.EncodeToMemory(&pem.Block{Type: "EC PRIVATE KEY", Bytes: kder}),
	)
	if err != nil {
		t.Fatal(err)
	}
	cert, _ := x509.ParseCertificate(der)
	pool := x509.NewCertPool()
	pool.AddCert(cert)
	return leaves, pool
}

// seen is what the upstream received.
type seen struct {
	mu   sync.Mutex
	reqs []*http.Request
}

func (s *seen) add(r *http.Request) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.reqs = append(s.reqs, r)
}

func (s *seen) all() []*http.Request {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.reqs
}

// echo answers with the request headers as JSON, like httpbin's /headers,
// and repeats each as a response header named X-Echo-<name>.
func echo(w http.ResponseWriter, r *http.Request) {
	h := map[string]string{}
	for k := range r.Header {
		h[k] = r.Header.Get(k)
		w.Header().Set("X-Echo-"+k, r.Header.Get(k))
	}
	_ = json.NewEncoder(w).Encode(h)
}

type harness struct {
	client   *http.Client
	upstream *seen
	audit    *auditBuf
	addr     string // the decrypter's listener
	pool     *x509.CertPool

	mu      sync.Mutex
	verdict policy.Verdict // what reauthorizing the flow returns
	caller  *Caller
}

// setCaller changes who the flow's grants name from the next request.
func (h *harness) setCaller(c *Caller) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.caller = c
}

// setVerdict changes what the flow's grants decide from the next request.
func (h *harness) setVerdict(v policy.Verdict) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.verdict = v
}

// newHarness puts an HTTPDecrypter for example.com, the name in
// httptest's certificate, between a client and handler.
func newHarness(t *testing.T, tlsOn bool, rules []*policy.HTTPRule, secrets SecretSource, handler http.HandlerFunc, opts ...func(*HTTPDecrypter)) *harness {
	t.Helper()
	h := &harness{upstream: &seen{}, audit: &auditBuf{}}
	up := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		h.upstream.add(r)
		handler(w, r)
	}))
	if tlsOn {
		up.StartTLS()
	} else {
		up.Start()
	}
	t.Cleanup(up.Close)
	roots := x509.NewCertPool()
	if tlsOn {
		roots.AddCert(up.Certificate())
	}
	leaves, pool := testCA(t)
	h.pool = pool
	d := &HTTPDecrypter{Leaves: leaves, Secrets: secrets, RootCAs: roots, Audit: slog.New(slog.NewJSONHandler(h.audit, nil))}
	for _, o := range opts {
		o(d)
	}
	v := policy.Decrypt{Rules: rules, Name: "example.com", TLS: tlsOn}
	h.verdict = v
	dst := netip.MustParseAddrPort("192.0.2.1:443")
	if !tlsOn {
		dst = netip.MustParseAddrPort("192.0.2.1:80")
	}
	dial := func(*Caller) DialFunc {
		return func(ctx context.Context, network, address string) (net.Conn, error) {
			if address != net.JoinHostPort("example.com", strconv.Itoa(int(dst.Port()))) {
				return nil, errors.New("dialled " + address)
			}
			return (&net.Dialer{}).DialContext(ctx, network, up.Listener.Addr().String())
		}
	}
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { ln.Close() })
	h.addr = ln.Addr().String()
	caller := &Caller{Node: "t3-alice", Tags: []string{"tag:t3-alice"}}
	h.caller = caller
	reauth := func(context.Context) (*Caller, policy.Verdict) {
		h.mu.Lock()
		defer h.mu.Unlock()
		return h.caller, h.verdict
	}
	go func() {
		for {
			c, err := ln.Accept()
			if err != nil {
				return
			}
			go func() {
				defer c.Close()
				_, _, _ = d.Serve(context.Background(), caller, v, reauth, dst, c, nil, dial)
			}()
		}
	}()
	h.client = &http.Client{Transport: &http.Transport{
		DialContext: func(ctx context.Context, network, _ string) (net.Conn, error) {
			return (&net.Dialer{}).DialContext(ctx, network, h.addr)
		},
		TLSClientConfig:    &tls.Config{RootCAs: pool},
		DisableCompression: true,
	}}
	t.Cleanup(h.client.CloseIdleConnections)
	return h
}

func (h *harness) do(t *testing.T, method, url string, hdr map[string]string) (int, string) {
	t.Helper()
	resp, body := h.response(t, method, url, hdr)
	return resp.StatusCode, body
}

func (h *harness) response(t *testing.T, method, url string, hdr map[string]string) (*http.Response, string) {
	t.Helper()
	req, err := http.NewRequest(method, url, nil)
	if err != nil {
		t.Fatal(err)
	}
	for k, v := range hdr {
		if k == "Host" {
			req.Host = v
			continue
		}
		req.Header.Set(k, v)
	}
	resp, err := h.client.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()
	b, err := io.ReadAll(resp.Body)
	if err != nil {
		t.Fatal(err)
	}
	return resp, string(b)
}

// requests returns the request audit lines, and fails if any line holds a
// secret value.
func (h *harness) requests(t *testing.T) []map[string]any {
	t.Helper()
	h.audit.mu.Lock()
	raw := h.audit.buf.String()
	h.audit.mu.Unlock()
	for _, s := range []string{dummySecret, ghSecret} {
		if strings.Contains(raw, s) {
			t.Fatalf("audit log holds a secret: %s", raw)
		}
	}
	var out []map[string]any
	for _, l := range h.audit.lines(t) {
		if l["msg"] == "request" {
			out = append(out, l)
		}
	}
	return out
}

func httpbinRule() []*policy.HTTPRule {
	return []*policy.HTTPRule{{ID: "g0.http1", Ports: []uint16{443}, Credentials: []policy.Credential{dummyCred}}}
}

// The tenant sends no X-Dummy-Token; the upstream gets the real value. Its
// echo in a response header comes back as the placeholder, and in the body
// as it was sent.
func TestDecryptSetsMissingHeader(t *testing.T) {
	h := newHarness(t, true, httpbinRule(), fakeSecrets{}, echo)
	resp, body := h.response(t, "GET", "https://example.com/headers?q=1", nil)
	if resp.StatusCode != 200 {
		t.Fatalf("status %d: %s", resp.StatusCode, body)
	}
	reqs := h.upstream.all()
	if len(reqs) != 1 || reqs[0].Header.Get("X-Dummy-Token") != dummySecret {
		t.Fatalf("upstream got %v", reqs)
	}
	if got := resp.Header.Get("X-Echo-X-Dummy-Token"); got != dummyCred.Placeholder() {
		t.Fatalf("echoed header = %q", got)
	}
	if !strings.Contains(body, dummySecret) {
		t.Fatalf("body changed: %s", body)
	}
	lines := h.requests(t)
	if len(lines) != 1 {
		t.Fatalf("request lines = %v", lines)
	}
	l := lines[0]
	if l["method"] != "GET" || l["host"] != "example.com" || l["path"] != "/headers" || l["rule"] != "g0.http1" ||
		l["status"] != float64(200) || toJSON(l["credentials"]) != `["DUMMY_TOKEN"]` || l["tags"] == nil {
		t.Fatalf("audit = %v", l)
	}
}

func TestDecryptSwapsPlaceholdersInHeaders(t *testing.T) {
	h := newHarness(t, true, httpbinRule(), fakeSecrets{}, echo)
	basic := base64.StdEncoding.EncodeToString([]byte("u:" + dummyCred.Placeholder()))
	resp, body := h.response(t, "GET", "https://example.com/headers", map[string]string{
		"X-Dummy-Token": "pre-" + dummyCred.Placeholder() + "-post",
		"Authorization": "Basic " + basic,
	})
	if resp.StatusCode != 200 {
		t.Fatalf("status %d: %s", resp.StatusCode, body)
	}
	got := h.upstream.all()[0].Header
	if got.Get("X-Dummy-Token") != "pre-"+dummySecret+"-post" {
		t.Fatalf("X-Dummy-Token = %q", got.Get("X-Dummy-Token"))
	}
	if u, p, _ := h.upstream.all()[0].BasicAuth(); u != "u" || p != dummySecret {
		t.Fatalf("basic auth = %q %q", u, p)
	}
	if got := resp.Header.Get("X-Echo-X-Dummy-Token"); got != "pre-"+dummyCred.Placeholder()+"-post" {
		t.Fatalf("echoed X-Dummy-Token = %q", got)
	}
	if got := resp.Header.Get("X-Echo-Authorization"); got != "Basic "+basic {
		t.Fatalf("echoed Authorization = %q", got)
	}
}

// Response header values lose each value the request injected, with or
// without its scheme prefix. A request that injected nothing gets the
// upstream's headers as they are, and no body is ever rewritten.
func TestDecryptScrubsResponseHeaders(t *testing.T) {
	rules := []*policy.HTTPRule{
		{ID: "cred", Ports: []uint16{443}, Paths: []string{"/cred"}, Credentials: []policy.Credential{ghCred}},
		{ID: "open", Ports: []uint16{443}, Paths: []string{"/open"}},
	}
	h := newHarness(t, true, rules, fakeSecrets{}, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("X-Full", "seen "+r.Header.Get("Authorization"))
		w.Header().Set("X-Bare", ghSecret)
		w.Header().Add("X-Bare", "other")
		_, _ = io.WriteString(w, ghSecret)
	})
	resp, body := h.response(t, "GET", "https://example.com/cred", nil)
	p := ghCred.Placeholder()
	if resp.Header.Get("X-Full") != "seen Bearer "+p || !slices.Equal(resp.Header.Values("X-Bare"), []string{p, "other"}) || body != ghSecret {
		t.Fatalf("headers %v, body %q", resp.Header, body)
	}
	resp, _ = h.response(t, "GET", "https://example.com/open", nil)
	if resp.Header.Get("X-Bare") != ghSecret {
		t.Fatalf("uncredentialed headers %v", resp.Header)
	}
}

// Where two values start at the same byte, the longer is replaced.
func TestScrubberPrefersTheLongerValue(t *testing.T) {
	s := scrubber{"secret": "P1", "secretive": "P2", "sss": "P3"}
	if got := s.string("xsecretxsecretivessssecresecret"); got != "xP1xP2P3secreP1" {
		t.Fatalf("got %q", got)
	}
}

// Each request is checked against every rule for the host: methods and
// paths pick the rules, and only their credentials may be used.
func TestDecryptMatchesEachRequestAcrossRules(t *testing.T) {
	read := &policy.HTTPRule{ID: "read", Methods: []string{"GET"}, Paths: []string{"/repos/*"}, Credentials: []policy.Credential{ghCred}}
	upload := &policy.HTTPRule{ID: "upload", Methods: []string{"POST"}, Paths: []string{"/upload"}, Credentials: []policy.Credential{dummyCred}}
	h := newHarness(t, true, []*policy.HTTPRule{read, upload}, fakeSecrets{}, echo)

	if code, _ := h.do(t, "GET", "https://example.com/repos/x", nil); code != 200 {
		t.Fatalf("GET /repos/x = %d", code)
	}
	if code, _ := h.do(t, "POST", "https://example.com/upload", nil); code != 200 {
		t.Fatalf("POST /upload = %d", code)
	}
	for _, tt := range []struct{ method, url, header, value string }{
		{"DELETE", "https://example.com/repos/x", "", ""},
		{"GET", "https://example.com/upload", "", ""},
		{"GET", "https://example.com/repos/x/y", "", ""},
		{"GET", "https://example.com/repos/x", "X-Dummy-Token", dummyCred.Placeholder()},
		{"GET", "https://example.com/repos/..", "", ""},
	} {
		hdr := map[string]string{}
		if tt.header != "" {
			hdr[tt.header] = tt.value
		}
		if code, _ := h.do(t, tt.method, tt.url, hdr); code != 403 {
			t.Fatalf("%s %s %v = %d, want 403", tt.method, tt.url, hdr, code)
		}
	}
	reqs := h.upstream.all()
	if len(reqs) != 2 {
		t.Fatalf("upstream got %d requests", len(reqs))
	}
	get, post := reqs[0].Header, reqs[1].Header
	if get.Get("Authorization") != "Bearer "+ghSecret || get.Get("X-Dummy-Token") != "" {
		t.Fatalf("GET headers = %v", get)
	}
	if post.Get("X-Dummy-Token") != dummySecret || post.Get("Authorization") != "" {
		t.Fatalf("POST headers = %v", post)
	}
	reasons := map[string]int{}
	for _, l := range h.requests(t) {
		if r, ok := l["reason"].(string); ok {
			reasons[r]++
		}
	}
	if reasons[reasonRequestNoRule] != 3 || reasons[reasonPlaceholderOther] != 1 || reasons[reasonDotSegment] != 1 {
		t.Fatalf("reasons = %v", reasons)
	}
}

// Refusals that never reach the upstream.
func TestDecryptRefuses(t *testing.T) {
	p := dummyCred.Placeholder()
	for name, tt := range map[string]struct {
		url    string
		hdr    map[string]string
		reason string
	}{
		"host is not sni":          {"https://example.com/", map[string]string{"Host": "evil.example"}, reasonHostMismatch},
		"host with stray colons":   {"https://example.com/", map[string]string{"Host": "example.com:443:443"}, reasonHostMismatch},
		"host with empty port":     {"https://example.com/", map[string]string{"Host": "example.com:"}, reasonHostMismatch},
		"placeholder in path":      {"https://example.com/anything/" + p, nil, reasonPlaceholderURL},
		"placeholder in query":     {"https://example.com/anything?t=" + p, nil, reasonPlaceholderURL},
		"encoded placeholder":      {"https://example.com/anything/%54S_EGRESS_PLACEHOLDER_DUMMY_TOKEN", nil, reasonPlaceholderURL},
		"placeholder in cookie":    {"https://example.com/", map[string]string{"Cookie": "t=" + p}, reasonPlaceholderHdr},
		"placeholder in xff":       {"https://example.com/", map[string]string{"X-Forwarded-For": p}, reasonPlaceholderHdr},
		"placeholder in forwarded": {"https://example.com/", map[string]string{"Forwarded": "for=" + p}, reasonPlaceholderHdr},
		"placeholder in via":       {"https://example.com/", map[string]string{"Via": "1.1 " + p}, reasonPlaceholderHdr},
		"placeholder in proxy-*":   {"https://example.com/", map[string]string{"Proxy-Custom": p}, reasonPlaceholderHdr},
		"another host's":           {"https://example.com/", map[string]string{"Authorization": "Bearer " + ghCred.Placeholder()}, reasonPlaceholderOther},
		"unknown placeholder":      {"https://example.com/", map[string]string{"X-Other": policy.PlaceholderPrefix + "NOPE"}, reasonPlaceholderOther},
		"placeholder name suffix":  {"https://example.com/", map[string]string{"X-Other": p + "_X"}, reasonPlaceholderOther},
	} {
		t.Run(name, func(t *testing.T) {
			h := newHarness(t, true, httpbinRule(), fakeSecrets{}, echo)
			if code, body := h.do(t, "GET", tt.url, tt.hdr); code != 403 {
				t.Fatalf("status %d: %s", code, body)
			}
			if n := len(h.upstream.all()); n != 0 {
				t.Fatalf("upstream got %d requests", n)
			}
			if l := h.requests(t); len(l) != 1 || l[0]["reason"] != tt.reason || l[0]["status"] != float64(403) {
				t.Fatalf("audit = %v", l)
			}
		})
	}
}

func TestDecryptFailsClosedWithoutTheSecret(t *testing.T) {
	h := newHarness(t, true, httpbinRule(), fakeSecrets{err: errors.New("connect answered 503")}, echo)
	if code, _ := h.do(t, "GET", "https://example.com/headers", nil); code != http.StatusBadGateway {
		t.Fatalf("status %d", code)
	}
	if n := len(h.upstream.all()); n != 0 {
		t.Fatalf("upstream got %d requests", n)
	}
	if l := h.requests(t); len(l) != 1 || l[0]["reason"] != reasonSecret || l[0]["error"] != "connect answered 503" {
		t.Fatalf("audit = %v", l)
	}
}

// Response bodies pass as the upstream sent them: the client's
// Accept-Encoding and Range reach the upstream, and the upstream's
// encoding, length and trailers reach the client.
func TestDecryptPassesResponseBodies(t *testing.T) {
	var gz bytes.Buffer
	zw := gzip.NewWriter(&gz)
	_, _ = io.WriteString(zw, "echo "+dummySecret)
	_ = zw.Close()
	h := newHarness(t, true, httpbinRule(), fakeSecrets{}, func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path == "/trailer" {
			w.Header().Set("Trailer", "X-Sum")
			_, _ = io.WriteString(w, "body")
			w.(http.Flusher).Flush()
			w.Header().Set("X-Sum", "done")
			return
		}
		w.Header().Set("Content-Encoding", "gzip")
		w.Header().Set("Content-Length", strconv.Itoa(gz.Len()))
		w.WriteHeader(http.StatusPartialContent)
		_, _ = w.Write(gz.Bytes())
	})
	resp, body := h.response(t, "GET", "https://example.com/gz", map[string]string{"Accept-Encoding": "gzip", "Range": "bytes=0-", "If-Range": `"v1"`})
	if resp.StatusCode != http.StatusPartialContent || resp.Header.Get("Content-Encoding") != "gzip" || resp.ContentLength != int64(gz.Len()) || body != gz.String() {
		t.Fatalf("status %d, headers %v, length %d", resp.StatusCode, resp.Header, resp.ContentLength)
	}
	up := h.upstream.all()[0].Header
	if up.Get("Accept-Encoding") != "gzip" || up.Get("Range") != "bytes=0-" || up.Get("If-Range") != `"v1"` {
		t.Fatalf("upstream got %v", up)
	}
	resp, body = h.response(t, "GET", "https://example.com/trailer", nil)
	if body != "body" || resp.Trailer.Get("X-Sum") != "done" {
		t.Fatalf("body %q, trailer %v", body, resp.Trailer)
	}
}

// An upstream whose certificate the roots do not vouch for gets nothing.
func TestDecryptVerifiesTheUpstream(t *testing.T) {
	h := newHarness(t, true, httpbinRule(), fakeSecrets{}, echo, func(d *HTTPDecrypter) { d.RootCAs = x509.NewCertPool() })
	if code, _ := h.do(t, "GET", "https://example.com/", nil); code != http.StatusBadGateway {
		t.Fatalf("status %d", code)
	}
	if n := len(h.upstream.all()); n != 0 {
		t.Fatalf("upstream got %d requests", n)
	}
	if l := h.requests(t); len(l) != 1 || l[0]["reason"] != reasonUpstreamTLS || l[0]["error"] != nil {
		t.Fatalf("audit = %v", l)
	}
}

// Plain HTTP on an http-rule port is proxied under the request rules but
// never gets a credential.
func TestPlainHTTPNeverGetsACredential(t *testing.T) {
	h := newHarness(t, false, []*policy.HTTPRule{{ID: "plain", Ports: []uint16{80}, Credentials: []policy.Credential{dummyCred}}}, fakeSecrets{}, echo)
	if code, body := h.do(t, "GET", "http://example.com/headers", nil); code != 200 || strings.Contains(body, "X-Dummy-Token") {
		t.Fatalf("status %d: %s", code, body)
	}
	if code, _ := h.do(t, "GET", "http://example.com/headers", map[string]string{"X-Dummy-Token": dummyCred.Placeholder()}); code != 403 {
		t.Fatalf("placeholder over plain HTTP: status %d", code)
	}
	if n := len(h.upstream.all()); n != 1 {
		t.Fatalf("upstream got %d requests", n)
	}
	if l := h.requests(t); len(l) != 2 || l[0]["credentials"] != nil || l[1]["reason"] != reasonPlaceholderPlain {
		t.Fatalf("audit = %v", l)
	}
}

// upgrader answers an Upgrade: test request with 101, repeating its
// X-Dummy-Token as X-Echo, and echoes what follows. Anything else gets the
// request headers.
func upgrader(w http.ResponseWriter, r *http.Request) {
	if r.Header.Get("Upgrade") != "test" {
		echo(w, r)
		return
	}
	c, brw, err := http.NewResponseController(w).Hijack()
	if err != nil {
		return
	}
	defer c.Close()
	_, _ = io.WriteString(c, "HTTP/1.1 101 Switching Protocols\r\nConnection: Upgrade\r\nUpgrade: test\r\nX-Echo: "+r.Header.Get("X-Dummy-Token")+"\r\n\r\n")
	_, _ = io.Copy(c, brw)
}

const upgradeReq = "GET /ws HTTP/1.1\r\nHost: example.com\r\nConnection: Upgrade\r\nUpgrade: test\r\n\r\n"

// An upgrade is spliced, a credentialed one too, and the 101's headers are
// scrubbed like any response's.
func TestDecryptPassesUpgrades(t *testing.T) {
	for name, rules := range map[string][]*policy.HTTPRule{
		"uncredentialed": {{ID: "ws", Ports: []uint16{443}}},
		"credentialed":   httpbinRule(),
	} {
		t.Run(name, func(t *testing.T) {
			h := newHarness(t, true, rules, fakeSecrets{}, upgrader)
			c, err := tls.Dial("tcp", h.addr, &tls.Config{ServerName: "example.com", RootCAs: h.pool})
			if err != nil {
				t.Fatal(err)
			}
			defer c.Close()
			_ = c.SetDeadline(time.Now().Add(5 * time.Second))
			_, _ = io.WriteString(c, upgradeReq)
			br := bufio.NewReader(c)
			resp, err := http.ReadResponse(br, nil)
			if err != nil || resp.StatusCode != 101 {
				t.Fatalf("upgrade: %v %v", resp, err)
			}
			if got := h.upstream.all()[0].Header.Get("X-Dummy-Token"); name == "credentialed" && (got != dummySecret || resp.Header.Get("X-Echo") != dummyCred.Placeholder()) {
				t.Fatalf("upstream got %q, client got X-Echo %q", got, resp.Header.Get("X-Echo"))
			}
			_, _ = io.WriteString(c, "ping")
			got := make([]byte, 4)
			if _, err := io.ReadFull(br, got); err != nil || !bytes.Equal(got, []byte("ping")) {
				t.Fatalf("after upgrade: %q %v", got, err)
			}
		})
	}
}

// An upstream that switches protocols unasked is refused.
func TestDecryptRefusesAnUnexpected101(t *testing.T) {
	h := newHarness(t, true, httpbinRule(), fakeSecrets{}, func(w http.ResponseWriter, r *http.Request) {
		c, _, err := http.NewResponseController(w).Hijack()
		if err != nil {
			return
		}
		defer c.Close()
		_, _ = io.WriteString(c, "HTTP/1.1 101 Switching Protocols\r\nConnection: Upgrade\r\nUpgrade: test\r\n\r\n"+r.Header.Get("X-Dummy-Token"))
		time.Sleep(100 * time.Millisecond)
	})
	raw := h.rawExchange(t, "GET / HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n\r\n")
	if strings.Contains(raw, dummySecret) || !strings.HasPrefix(raw, "HTTP/1.1 502") {
		t.Fatalf("client got %q", raw)
	}
	if l := h.requests(t); len(l) != 1 || l[0]["reason"] != reasonUnexpected101 {
		t.Fatalf("audit = %v", l)
	}
}

// The upstream's bytes never reach the audit log through an error.
func TestDecryptAuditCarriesNoUpstreamBytes(t *testing.T) {
	const marker = "LEAKMARK"
	h := newHarness(t, true, httpbinRule(), fakeSecrets{}, func(w http.ResponseWriter, _ *http.Request) {
		c, _, err := http.NewResponseController(w).Hijack()
		if err != nil {
			return
		}
		defer c.Close()
		_, _ = io.WriteString(c, "HTTP/1.1 200 OK\r\nX-Bad "+marker+"\r\n\r\n")
	})
	if code, _ := h.do(t, "GET", "https://example.com/", nil); code != http.StatusBadGateway {
		t.Fatalf("status %d", code)
	}
	h.audit.mu.Lock()
	raw := h.audit.buf.String()
	h.audit.mu.Unlock()
	if strings.Contains(raw, marker) {
		t.Fatalf("audit holds upstream bytes: %s", raw)
	}
	if l := h.requests(t); len(l) != 1 || l[0]["reason"] != reasonUpstream || l[0]["error"] != nil {
		t.Fatalf("audit = %v", l)
	}
}

// rawExchange sends one raw request over TLS and returns every byte the
// client gets back until the connection closes.
func (h *harness) rawExchange(t *testing.T, req string) string {
	t.Helper()
	c, err := tls.Dial("tcp", h.addr, &tls.Config{ServerName: "example.com", RootCAs: h.pool})
	if err != nil {
		t.Fatal(err)
	}
	defer c.Close()
	_ = c.SetDeadline(time.Now().Add(5 * time.Second))
	if _, err := io.WriteString(c, req); err != nil {
		t.Fatal(err)
	}
	b, _ := io.ReadAll(c)
	return string(b)
}

// Informational responses never reach the client: ReverseProxy would copy
// their headers before any scrubbing.
func TestDecryptDropsInformationalResponses(t *testing.T) {
	h := newHarness(t, true, httpbinRule(), fakeSecrets{}, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Link", "</x>; rel=preload; v="+r.Header.Get("X-Dummy-Token"))
		w.WriteHeader(http.StatusEarlyHints)
		w.Header().Del("Link")
		_, _ = io.WriteString(w, "final")
	})
	raw := h.rawExchange(t, "GET / HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n\r\n")
	if strings.Contains(raw, dummySecret) || strings.Contains(raw, "103") || !strings.HasPrefix(raw, "HTTP/1.1 200") || !strings.Contains(raw, "final") {
		t.Fatalf("client got %q", raw)
	}
}

// A header name cannot be rewritten, so one holding a secret is refused.
func TestDecryptRefusesASecretInAHeaderName(t *testing.T) {
	h := newHarness(t, true, httpbinRule(), fakeSecrets{}, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("X-"+r.Header.Get("X-Dummy-Token"), "1")
		_, _ = io.WriteString(w, "body")
	})
	code, body := h.do(t, "GET", "https://example.com/", nil)
	if code != http.StatusBadGateway || strings.Contains(body, "body") {
		t.Fatalf("status %d: %s", code, body)
	}
	if l := h.requests(t); len(l) != 1 || l[0]["reason"] != reasonSecretHdrName {
		t.Fatalf("audit = %v", l)
	}
}

// Each request on a live connection is decided under the grants that hold
// now: a rotated secret reference is used at once, and a revoked grant
// refuses the request and ends the connection.
func TestDecryptReauthorizesEachRequest(t *testing.T) {
	h := newHarness(t, true, httpbinRule(), fakeSecrets{}, echo)
	var conns atomic.Int32
	h.client.Transport.(*http.Transport).DialContext = func(ctx context.Context, network, _ string) (net.Conn, error) {
		conns.Add(1)
		return (&net.Dialer{}).DialContext(ctx, network, h.addr)
	}
	if code, _ := h.do(t, "GET", "https://example.com/", nil); code != 200 {
		t.Fatalf("first: status %d", code)
	}
	rotated := dummyCred
	rotated.Secret.Ref = "rotated-value"
	h.setVerdict(policy.Decrypt{Name: "example.com", TLS: true, Rules: []*policy.HTTPRule{{ID: "g1.http1", Ports: []uint16{443}, Credentials: []policy.Credential{rotated}}}})
	if code, body := h.do(t, "GET", "https://example.com/", nil); code != 200 {
		t.Fatalf("rotated: status %d: %s", code, body)
	}
	h.setVerdict(policy.Deny{Reason: policy.ReasonNoRule})
	req, _ := http.NewRequest("GET", "https://example.com/", nil)
	resp, err := h.client.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	resp.Body.Close()
	if resp.StatusCode != http.StatusForbidden || !resp.Close {
		t.Fatalf("revoked: status %d, close %v", resp.StatusCode, resp.Close)
	}
	reqs := h.upstream.all()
	if conns.Load() != 1 || len(reqs) != 2 || reqs[0].Header.Get("X-Dummy-Token") != dummySecret || reqs[1].Header.Get("X-Dummy-Token") != "rotated-value" {
		t.Fatalf("%d client conns, upstream got %v", conns.Load(), reqs)
	}
	l := h.requests(t)
	if len(l) != 3 || l[1]["rule"] != "g1.http1" || l[2]["reason"] != reasonRevoked+":"+string(policy.ReasonNoRule) || l[2]["status"] != float64(403) {
		t.Fatalf("audit = %v", l)
	}
}

// Rules match the decoded path. Where a credential would be injected, a
// target that decodes to something other than what the upstream may read
// is refused, and what is forwarded is the canonical target that matched.
func TestDecryptRefusesAmbiguousCredentialedTargets(t *testing.T) {
	rules := []*policy.HTTPRule{
		{ID: "cred", Ports: []uint16{443}, Paths: []string{"/repos/*/*"}, Credentials: []policy.Credential{dummyCred}},
		{ID: "open", Ports: []uint16{443}, Paths: []string{"/open/*/*"}},
	}
	for _, target := range []string{"/repos/a%2Fb", "/repos/a%2fb", "/repos/a%5Cb/c", "/repos/a%5cb/c", "/repos/%61/b", "/repos/a/%2e%2e", "/repos//b"} {
		t.Run(target, func(t *testing.T) {
			h := newHarness(t, true, rules, fakeSecrets{}, echo)
			raw := h.rawExchange(t, "GET "+target+" HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n\r\n")
			if !strings.HasPrefix(raw, "HTTP/1.1 403") || len(h.upstream.all()) != 0 {
				t.Fatalf("got %q, upstream got %d requests", raw, len(h.upstream.all()))
			}
			if l := h.requests(t); len(l) != 1 || (l[0]["reason"] != reasonAmbiguousPath && l[0]["reason"] != reasonDotSegment) {
				t.Fatalf("audit = %v", l)
			}
		})
	}
	for _, target := range []string{"foo:bar", "*"} {
		t.Run("any path "+target, func(t *testing.T) {
			h := newHarness(t, true, httpbinRule(), fakeSecrets{}, echo)
			raw := h.rawExchange(t, "GET "+target+" HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n\r\n")
			if reqs := h.upstream.all(); len(reqs) != 0 {
				t.Fatalf("got %q, upstream got %q", raw, reqs[0].RequestURI)
			}
		})
	}
	for target, want := range map[string]string{
		"/repos/a%20b/c?x=%2F": "/repos/a%20b/c?x=%2F",
		"/open/a%2Fb":          "/open/a%2Fb",
	} {
		t.Run(target, func(t *testing.T) {
			h := newHarness(t, true, rules, fakeSecrets{}, echo)
			raw := h.rawExchange(t, "GET "+target+" HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n\r\n")
			reqs := h.upstream.all()
			if !strings.HasPrefix(raw, "HTTP/1.1 200") || len(reqs) != 1 || reqs[0].RequestURI != want {
				t.Fatalf("got %q, upstream got %v", raw, reqs)
			}
		})
	}
}

func shortTimeouts(d *HTTPDecrypter) {
	d.BodyIdle, d.HeaderTimeout = 200*time.Millisecond, 200*time.Millisecond
}

// readsBody answers with the length of the request body, then streams
// five chunks 150ms apart, longer than either timeout.
func readsBody(w http.ResponseWriter, r *http.Request) {
	b, err := io.ReadAll(r.Body)
	if err != nil {
		return
	}
	_, _ = io.WriteString(w, strconv.Itoa(len(b))+":")
	for range 5 {
		w.(http.Flusher).Flush()
		time.Sleep(150 * time.Millisecond)
		_, _ = io.WriteString(w, "x")
	}
}

// A request body that stops arriving ends the request; one that keeps
// arriving, slower than the idle bound in total, does not, and neither
// timeout cuts a response that streams for longer than both.
func TestDecryptBoundsRequestBodiesNotStreams(t *testing.T) {
	t.Run("stalled", func(t *testing.T) {
		h := newHarness(t, true, httpbinRule(), fakeSecrets{}, readsBody, shortTimeouts)
		start := time.Now()
		raw := h.rawExchange(t, "POST / HTTP/1.1\r\nHost: example.com\r\nContent-Length: 10\r\n\r\nabc")
		if time.Since(start) > 2*time.Second || !strings.HasPrefix(raw, "HTTP/1.1 502") {
			t.Fatalf("after %v got %q", time.Since(start), raw)
		}
		if l := h.requests(t); len(l) != 1 || l[0]["reason"] != reasonBodyIdle {
			t.Fatalf("audit = %v", l)
		}
	})
	t.Run("trickling", func(t *testing.T) {
		h := newHarness(t, true, httpbinRule(), fakeSecrets{}, readsBody, shortTimeouts)
		pr, pw := io.Pipe()
		go func() {
			for range 6 {
				time.Sleep(100 * time.Millisecond)
				_, _ = io.WriteString(pw, "ab")
			}
			pw.Close()
		}()
		req, _ := http.NewRequest("POST", "https://example.com/", pr)
		resp, err := h.client.Do(req)
		if err != nil {
			t.Fatal(err)
		}
		defer resp.Body.Close()
		b, err := io.ReadAll(resp.Body)
		if err != nil || resp.StatusCode != 200 || string(b) != "12:xxxxx" {
			t.Fatalf("status %d: %q %v", resp.StatusCode, b, err)
		}
	})
}

// A refused request's body may be drained before the refusal is sent, and
// an approved one written to an upstream that stops reading it. Neither
// wait outlasts the idle bound.
func TestDecryptBoundsBodiesOutsideTheReader(t *testing.T) {
	t.Run("refused", func(t *testing.T) {
		h := newHarness(t, true, httpbinRule(), fakeSecrets{}, echo, shortTimeouts)
		start := time.Now()
		raw := h.rawExchange(t, "POST /?x="+dummyCred.Placeholder()+" HTTP/1.1\r\nHost: example.com\r\nContent-Length: 10\r\n\r\nabc")
		if time.Since(start) > 2*time.Second || !strings.HasPrefix(raw, "HTTP/1.1 403") {
			t.Fatalf("after %v got %q", time.Since(start), raw)
		}
	})
	t.Run("upstream stops reading", func(t *testing.T) {
		stop := make(chan struct{})
		h := newHarness(t, true, httpbinRule(), fakeSecrets{}, func(w http.ResponseWriter, _ *http.Request) {
			c, _, err := http.NewResponseController(w).Hijack()
			if err != nil {
				return
			}
			defer c.Close()
			<-stop
		}, shortTimeouts)
		t.Cleanup(func() { close(stop) })
		h.client.Timeout = 5 * time.Second
		req, _ := http.NewRequest("POST", "https://example.com/", io.LimitReader(zeros{}, 1<<30))
		start := time.Now()
		resp, err := h.client.Do(req)
		if err != nil {
			t.Fatalf("after %v: %v", time.Since(start), err)
		}
		resp.Body.Close()
		if resp.StatusCode != http.StatusBadGateway || time.Since(start) > 2*time.Second {
			t.Fatalf("status %d after %v", resp.StatusCode, time.Since(start))
		}
		if l := h.requests(t); len(l) != 1 || l[0]["status"] != float64(http.StatusBadGateway) {
			t.Fatalf("audit = %v", l)
		}
	})
}

type zeros struct{}

func (zeros) Read(p []byte) (int, error) {
	clear(p)
	return len(p), nil
}

// An upstream that never answers is a 502 once its headers are overdue.
func TestDecryptBoundsUpstreamHeaders(t *testing.T) {
	h := newHarness(t, true, httpbinRule(), fakeSecrets{}, func(w http.ResponseWriter, _ *http.Request) {
		time.Sleep(time.Second)
	}, shortTimeouts)
	start := time.Now()
	if code, _ := h.do(t, "GET", "https://example.com/", nil); code != http.StatusBadGateway || time.Since(start) > 900*time.Millisecond {
		t.Fatalf("status %d after %v", code, time.Since(start))
	}
	if l := h.requests(t); len(l) != 1 || l[0]["reason"] != reasonUpstreamTimeout {
		t.Fatalf("audit = %v", l)
	}
}

// A failed response copy can quote the upstream's bytes, here a malformed
// trailer line holding the secret. No log level gets them.
func TestDecryptProxyErrorsLogNoUpstreamBytes(t *testing.T) {
	var logs auditBuf
	prev := slog.Default()
	slog.SetDefault(slog.New(slog.NewTextHandler(&logs, &slog.HandlerOptions{Level: slog.LevelDebug})))
	t.Cleanup(func() { slog.SetDefault(prev) })
	h := newHarness(t, true, httpbinRule(), fakeSecrets{}, func(w http.ResponseWriter, r *http.Request) {
		c, _, err := http.NewResponseController(w).Hijack()
		if err != nil {
			return
		}
		defer c.Close()
		_, _ = io.WriteString(c, "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n4\r\nbody\r\n0\r\nX-Bad "+r.Header.Get("X-Dummy-Token")+"\r\n\r\n")
	})
	raw := h.rawExchange(t, "GET / HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n\r\n")
	if strings.Contains(raw, dummySecret) {
		t.Fatalf("client got %q", raw)
	}
	logs.mu.Lock()
	defer logs.mu.Unlock()
	if s := logs.buf.String(); strings.Contains(s, dummySecret) || !strings.Contains(s, "level=DEBUG") {
		t.Fatalf("debug log = %q", s)
	}
}

// ReverseProxy drops the headers a client names in Connection. The
// credential the egress sets still reaches the upstream.
func TestDecryptKeepsHeadersTheClientNamesInConnection(t *testing.T) {
	h := newHarness(t, true, httpbinRule(), fakeSecrets{}, echo)
	raw := h.rawExchange(t, "GET /headers HTTP/1.1\r\nHost: example.com\r\nX-Dummy-Token: "+dummyCred.Placeholder()+"\r\nConnection: close, X-Dummy-Token\r\n\r\n")
	if !strings.HasPrefix(raw, "HTTP/1.1 200") {
		t.Fatalf("client got %q", raw)
	}
	if got := h.upstream.all()[0].Header; got.Get("X-Dummy-Token") != dummySecret {
		t.Fatalf("upstream got %v", got)
	}
}

// parsedConn reports when the server has read a request's head, which is
// when it clears the read deadline, just before it runs the handler.
type parsedConn struct {
	net.Conn
	parsed chan struct{}
	once   sync.Once
}

func (c *parsedConn) SetReadDeadline(t time.Time) error {
	if t.IsZero() {
		c.once.Do(func() { close(c.parsed) })
	}
	return c.Conn.SetReadDeadline(t)
}

// A request read just as the flow's context ends is handled before
// serveOne returns, or not at all.
func TestServeOneRunsNoHandlerAfterItReturns(t *testing.T) {
	for range 2000 {
		client, server := net.Pipe()
		conn := &parsedConn{Conn: server, parsed: make(chan struct{})}
		ctx, cancel := context.WithCancel(context.Background())
		var returned, late atomic.Bool
		h := http.HandlerFunc(func(http.ResponseWriter, *http.Request) { late.Store(returned.Load()) })
		done := make(chan struct{})
		go func() {
			serveOne(ctx, conn, h)
			returned.Store(true)
			close(done)
		}()
		go func() { _, _ = io.WriteString(client, "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n") }()
		<-conn.parsed
		cancel()
		<-done
		time.Sleep(100 * time.Microsecond)
		client.Close()
		if late.Load() {
			t.Fatal("a handler ran after serveOne returned")
		}
	}
}
