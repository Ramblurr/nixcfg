package egress

import (
	"bytes"
	"cmp"
	"context"
	"crypto/tls"
	"crypto/x509"
	"encoding/base64"
	"errors"
	"fmt"
	"io"
	"log"
	"log/slog"
	"maps"
	"net"
	"net/http"
	"net/http/httputil"
	"net/netip"
	"net/url"
	"path"
	"reflect"
	"slices"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"time"

	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/broker"
	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/policy"
)

// SecretSource resolves a credential's secret. Errors go to the audit log,
// so they never carry the value or bytes from the secret server's answers.
type SecretSource interface {
	Get(ctx context.Context, ref policy.SecretRef) (string, error)
}

// HTTPDecrypter serves Decrypt verdicts. On TLS it terminates the client
// with a leaf for the flow's name and re-encrypts to the real server,
// swapping placeholders for secrets on the way out, and secrets for
// placeholders in the response headers of a request that carried one.
// Response bodies pass as the upstream sent them. Plain HTTP is proxied
// under the same request rules but never gets a credential.
type HTTPDecrypter struct {
	Leaves  *Leaves
	Secrets SecretSource
	// Broker decides placeholders no rule lists, for callers whose caps
	// carry a broker ceiling. Nil refuses them.
	Broker *broker.Client
	// RootCAs verifies upstream servers. Nil means the system roots.
	RootCAs *x509.CertPool
	Audit   *slog.Logger
	// BodyIdle bounds each wait to read request body bytes from the client
	// or write them upstream, HeaderTimeout the wait for the upstream's
	// response headers. Zero means a minute and ten minutes. Neither bounds
	// a response body, which may stream.
	BodyIdle, HeaderTimeout time.Duration
}

// Request denial reasons, logged on the request audit line.
const (
	reasonHostMismatch         = "host-not-sni"
	reasonDotSegment           = "dot-segment"
	reasonRequestNoRule        = "no-request-rule"
	reasonPlaceholderURL       = "placeholder-in-url"
	reasonPlaceholderHdr       = "placeholder-in-protected-header"
	reasonPlaceholderOther     = "placeholder-not-allowed"
	reasonPlaceholderElsewhere = "placeholder-outside-inject-header"
	reasonPlaceholderPlain     = "placeholder-over-plain-http"
	reasonSecret               = "secret-unavailable"
	reasonSecretHdrName        = "secret-in-response-header-name"
	reasonUnexpected101        = "unexpected-101"
	reasonRevoked              = "grant-revoked"
	reasonAmbiguousPath        = "ambiguous-path"
	reasonBodyIdle             = "request-body-idle"
	reasonUpstream             = "upstream-failed"
	reasonUpstreamTLS          = "upstream-tls"
	reasonUpstreamTimeout      = "upstream-timeout"
	reasonApprovalPending      = "approval-pending"
	// reasonBrokerUnavailable is the broker deny reason logged when it
	// could not be asked or gave an answer the egress cannot use.
	reasonBrokerUnavailable = "broker-unavailable"
)

var (
	errSecretHdrName = errors.New("upstream put a secret in a header name")
	errUnexpected101 = errors.New("upstream switched protocols unasked")
)

func (d *HTTPDecrypter) Serve(ctx context.Context, caller *Caller, v policy.Decrypt, reauth Reauthorize, dst netip.AddrPort, client net.Conn, peeked []byte, dial func(*Caller) DialFunc) (up, down int64, err error) {
	cc := &countingConn{Conn: client, r: io.MultiReader(bytes.NewReader(peeked), client)}
	defer func() { up, down = cc.in.Load(), cc.out.Load() }()
	var conn net.Conn = cc
	if v.TLS {
		tc := tls.Server(cc, &tls.Config{
			MinVersion: tls.VersionTLS12,
			NextProtos: []string{"http/1.1"},
			GetCertificate: func(h *tls.ClientHelloInfo) (*tls.Certificate, error) {
				if policy.NormalizeName(h.ServerName) != v.Name {
					return nil, fmt.Errorf("SNI %q is not %q", h.ServerName, v.Name)
				}
				return d.Leaves.For(v.Name)
			},
		})
		hctx, cancel := context.WithTimeout(ctx, 10*time.Second)
		err := tc.HandshakeContext(hctx)
		cancel()
		if err != nil {
			return 0, 0, fmt.Errorf("client handshake: %w", err)
		}
		conn = tc
	}

	scheme, defPort := "http", uint16(80)
	if v.TLS {
		scheme, defPort = "https", 443
	}
	authority := v.Name
	if dst.Port() != defPort {
		authority = net.JoinHostPort(v.Name, strconv.Itoa(int(dst.Port())))
	}
	f := &decryptFlow{d: d, caller: caller, v: v, reauth: reauth, dial: dial, port: strconv.Itoa(int(dst.Port())), src: addrPort(client.RemoteAddr()), scheme: scheme, authority: authority}
	f.transport = f.newTransport(caller)
	serveOne(ctx, conn, f)
	f.transport.CloseIdleConnections()
	return 0, 0, nil
}

// newTransport dials upstream through caller's guard. A flow keeps one per
// policy, so no connection outlives the policy it was dialled under.
func (f *decryptFlow) newTransport(caller *Caller) *http.Transport {
	dial := f.dial(caller)
	upstream := net.JoinHostPort(f.v.Name, f.port)
	idle := cmp.Or(f.d.BodyIdle, time.Minute)
	return &http.Transport{
		// Dial the name, never the client's address, through the guard.
		DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
			c, err := dial(ctx, "tcp", upstream)
			if err != nil {
				return nil, err
			}
			return idleWriteConn{c, idle}, nil
		},
		TLSClientConfig:       &tls.Config{ServerName: f.v.Name, RootCAs: f.d.RootCAs, NextProtos: []string{"http/1.1"}},
		TLSHandshakeTimeout:   10 * time.Second,
		ResponseHeaderTimeout: cmp.Or(f.d.HeaderTimeout, 10*time.Minute),
		// Pass the client's Accept-Encoding and the encoded body as they are.
		DisableCompression:  true,
		MaxIdleConnsPerHost: 2,
		IdleConnTimeout:     time.Minute,
	}
}

// decryptFlow is one decrypted client connection.
type decryptFlow struct {
	d         *HTTPDecrypter
	caller    *Caller
	v         policy.Decrypt
	reauth    Reauthorize
	dial      func(*Caller) DialFunc
	port      string
	src       netip.AddrPort
	scheme    string
	authority string
	transport *http.Transport
}

// reqLog is one request audit line.
type reqLog struct {
	method, path string
	rules, creds []string
	broker       []brokerLog
	scrub        scrubber
	status       int
	reason       string
	err          error
	// pending is the approval a refused request waits for.
	pending string
}

// brokerLog is one broker decision on a request audit line.
type brokerLog struct {
	Credential string `json:"credential"`
	Decision   string `json:"decision"`
	Reason     string `json:"reason,omitempty"`
	Approval   string `json:"approval,omitempty"`
}

func (f *decryptFlow) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	l := &reqLog{method: r.Method, path: r.URL.Path}
	defer f.log(l)
	// The server may drain what is left of the body, outside idleBody: a
	// refused request's before the refusal is sent, any other's after.
	rc := http.NewResponseController(w)
	idle := cmp.Or(f.d.BodyIdle, time.Minute)
	defer func() { _ = rc.SetReadDeadline(time.Now().Add(idle)) }()
	if reason := f.reauthorize(r.Context()); reason != "" {
		l.status, l.reason = http.StatusForbidden, reason
		w.Header().Set("Connection", "close")
		http.Error(w, "ts-egress: "+reason, http.StatusForbidden)
		return
	}
	upgrade := r.Header.Get("Upgrade") != ""
	set, status, reason := f.prepare(r, l)
	if status != 0 {
		l.status, l.reason = status, reason
		// After a refused upgrade the client may already be speaking the
		// protocol it asked for.
		if upgrade {
			w.Header().Set("Connection", "close")
		}
		msg := "ts-egress: " + reason
		if l.pending != "" {
			msg = reasonApprovalPending + " " + l.pending
		}
		http.Error(w, msg, status)
		return
	}
	var body *idleBody
	if r.Body != nil && r.Body != http.NoBody {
		body = &idleBody{ReadCloser: r.Body, rc: rc, idle: idle}
		r.Body = body
	}
	rp := &httputil.ReverseProxy{
		Rewrite: func(pr *httputil.ProxyRequest) {
			pr.Out.URL.Scheme, pr.Out.URL.Host, pr.Out.URL.User = f.scheme, f.authority, nil
			pr.Out.Host = f.authority
			// ReverseProxy has dropped the headers the client named in
			// Connection, which may be ones the egress set.
			maps.Copy(pr.Out.Header, set)
		},
		Transport: f.transport,
		// ReverseProxy calls it after dropping hop-by-hop headers, and for a
		// 101 before it copies the headers out, so these are the headers the
		// client gets.
		ModifyResponse: func(resp *http.Response) error {
			l.status = resp.StatusCode
			if resp.StatusCode == http.StatusSwitchingProtocols && !upgrade {
				return errUnexpected101
			}
			for name := range resp.Header {
				if l.scrub.inName(name) {
					return errSecretHdrName
				}
			}
			l.scrub.header(resp.Header)
			return nil
		},
		ErrorHandler: func(w http.ResponseWriter, _ *http.Request, err error) {
			l.status, l.reason = http.StatusBadGateway, upstreamReason(err)
			if body != nil && body.timedOut.Load() {
				l.reason = reasonBodyIdle
			}
			w.WriteHeader(http.StatusBadGateway)
		},
		ErrorLog: log.New(fixedDebug("ts-egress: proxy error"), "", 0),
	}
	rp.ServeHTTP(noInformational{w}, r)
}

// fixedDebug logs itself at debug level for every line written to it. It
// takes ReverseProxy's errors, which can quote the upstream's bytes, and so
// a secret, and logs none of their text.
type fixedDebug string

func (m fixedDebug) Write(p []byte) (int, error) {
	slog.Debug(string(m))
	return len(p), nil
}

// idleBody gives each read of a request body its own deadline. It clears
// the deadline at the body's end, as the server then reads the connection
// in the background for the rest of the response and must not time out.
// After a timeout the deadline stays, so the server's drain of what is
// left is bounded too.
type idleBody struct {
	io.ReadCloser
	rc       *http.ResponseController
	idle     time.Duration
	timedOut atomic.Bool
}

func (b *idleBody) Read(p []byte) (int, error) {
	_ = b.rc.SetReadDeadline(time.Now().Add(b.idle))
	n, err := b.ReadCloser.Read(p)
	var ne net.Error
	switch {
	case errors.As(err, &ne) && ne.Timeout():
		b.timedOut.Store(true)
	case err != nil:
		_ = b.rc.SetReadDeadline(time.Time{})
	}
	return n, err
}

// idleWriteConn gives each write upstream its own deadline. The
// transport's header timer starts only once the request is written, so an
// upstream that stops reading a body would otherwise hold it for good.
type idleWriteConn struct {
	net.Conn
	idle time.Duration
}

func (c idleWriteConn) Write(p []byte) (int, error) {
	_ = c.SetWriteDeadline(time.Now().Add(c.idle))
	return c.Conn.Write(p)
}

// reauthorize takes up the caller and rules that hold now, so a request
// is matched, its secrets fetched and its upstream dialled under the
// current grants. It returns a refusal reason when the flow is no longer
// decrypted for this name.
func (f *decryptFlow) reauthorize(ctx context.Context) string {
	caller, v := f.reauth(ctx)
	switch v := v.(type) {
	case policy.Decrypt:
		if v.Name == f.v.Name && v.TLS == f.v.TLS {
			if !reflect.DeepEqual(caller.Policy, f.caller.Policy) {
				// Requests on a connection run one at a time, so every
				// upstream connection is idle now.
				f.transport.CloseIdleConnections()
				f.transport = f.newTransport(caller)
			}
			f.caller, f.v = caller, v
			return ""
		}
	case policy.Deny:
		return reasonRevoked + ":" + string(v.Reason)
	}
	return reasonRevoked
}

// noInformational drops the 1xx responses ReverseProxy relays as they
// arrive, before ModifyResponse could scrub their headers. 101 still
// passes: it is the final response to an upgrade.
type noInformational struct{ http.ResponseWriter }

func (w noInformational) WriteHeader(code int) {
	if code >= 100 && code < 200 && code != http.StatusSwitchingProtocols {
		return
	}
	w.ResponseWriter.WriteHeader(code)
}

func (w noInformational) Unwrap() http.ResponseWriter { return w.ResponseWriter }

// prepare checks r against the flow's rules and swaps its credentials in
// place. It returns the headers it set, which the outgoing request must
// carry as they are. A non-zero status refuses the request.
func (f *decryptFlow) prepare(r *http.Request, l *reqLog) (http.Header, int, string) {
	if host, ok := policy.ParseHost(r.Host); !ok || host != f.v.Name {
		return nil, http.StatusForbidden, reasonHostMismatch
	}
	if hasDotSegment(r.URL.Path) {
		return nil, http.StatusForbidden, reasonDotSegment
	}
	query, _ := url.QueryUnescape(r.URL.RawQuery)
	for _, s := range []string{r.RequestURI, r.URL.Path, query} {
		if strings.Contains(s, policy.PlaceholderPrefix) {
			return nil, http.StatusForbidden, reasonPlaceholderURL
		}
	}
	var allowed []policy.Credential
	for _, rule := range f.v.Rules {
		if ruleAllows(rule, r.Method, r.URL.Path) {
			l.rules = append(l.rules, rule.ID)
			for _, c := range rule.Credentials {
				if !slices.ContainsFunc(allowed, func(a policy.Credential) bool { return a.Name == c.Name }) {
					allowed = append(allowed, c)
				}
			}
		}
	}
	if len(l.rules) == 0 {
		return nil, http.StatusForbidden, reasonRequestNoRule
	}
	if !f.v.TLS {
		allowed = nil
	}
	ask, brokered := f.brokerRequest()
	need, unknown, reason := needed(r.Header, allowed, brokered)
	if reason != "" {
		if !f.v.TLS {
			reason = reasonPlaceholderPlain
		}
		return nil, http.StatusForbidden, reason
	}
	if slices.ContainsFunc(unknown, func(u unlisted) bool { return f.static(u.name) }) {
		return nil, http.StatusForbidden, reasonPlaceholderOther
	}
	if (len(allowed) > 0 || len(unknown) > 0) && ambiguousPath(r.URL) {
		return nil, http.StatusForbidden, reasonAmbiguousPath
	}
	for _, u := range unknown {
		c, status, reason := f.brokered(r.Context(), ask, u, l)
		if status != 0 {
			return nil, status, reason
		}
		if c.Inject.Header != u.header {
			return nil, http.StatusForbidden, reasonPlaceholderElsewhere
		}
		need = append(need, c)
	}
	values := map[string]string{}
	for _, c := range need {
		v, err := f.d.Secrets.Get(r.Context(), c.Secret)
		if err != nil {
			l.err = err
			return nil, http.StatusBadGateway, reasonSecret
		}
		values[c.Name] = v
		l.creds = append(l.creds, c.Name)
	}
	set, scrub := apply(r.Header, need, values)
	l.scrub = scrub
	return set, 0, ""
}

// brokerRequest returns the decide request for this flow's caller, without
// the credential and header. It is false for plain HTTP, for a caller
// without a broker ceiling for inject or without an identity, and when no
// broker is set.
func (f *decryptFlow) brokerRequest() (broker.Request, bool) {
	ceiling, identity := f.caller.Policy.Broker, f.caller.Identity()
	if !f.v.TLS || f.d.Broker == nil || ceiling == nil || !slices.Contains(ceiling.Modes, policy.BrokerInject) || identity == "" {
		return broker.Request{}, false
	}
	return broker.Request{
		Identity: identity, Node: f.caller.Node, Groups: ceiling.Groups, Modes: ceiling.Modes,
		Host: f.v.Name, Mode: policy.BrokerInject,
	}, true
}

func (f *decryptFlow) static(name string) bool {
	return slices.ContainsFunc(f.caller.Policy.HTTP, func(r policy.HTTPRule) bool {
		return slices.ContainsFunc(r.Credentials, func(c policy.Credential) bool { return c.Name == name })
	})
}

func (f *decryptFlow) brokered(ctx context.Context, req broker.Request, u unlisted, l *reqLog) (policy.Credential, int, string) {
	req.Credential, req.Header = u.name, u.header
	v, err := f.d.Broker.Decide(ctx, req)
	if err != nil {
		l.err = err
		v = broker.Deny{Reason: reasonBrokerUnavailable}
	}
	switch v := v.(type) {
	case broker.Allow:
		l.broker = append(l.broker, brokerLog{Credential: u.name, Decision: "allow", Approval: v.Approval})
		return v.Credential, 0, ""
	case broker.Pending:
		l.broker = append(l.broker, brokerLog{Credential: u.name, Decision: "pending", Approval: v.Approval})
		l.pending = v.Approval
		return policy.Credential{}, http.StatusForbidden, reasonApprovalPending
	case broker.Deny:
		l.broker = append(l.broker, brokerLog{Credential: u.name, Decision: "deny", Reason: v.Reason})
	}
	return policy.Credential{}, http.StatusForbidden, reasonPlaceholderOther
}

// upstreamReason classes a failed round trip. The error itself is not
// logged: transport errors quote the upstream's bytes and certificates.
func upstreamReason(err error) string {
	var de *policy.DenyError
	var cv *tls.CertificateVerificationError
	var ne net.Error
	switch {
	case errors.Is(err, errSecretHdrName):
		return reasonSecretHdrName
	case errors.Is(err, errUnexpected101):
		return reasonUnexpected101
	case errors.As(err, &de):
		return string(de.Reason)
	case errors.As(err, &cv):
		return reasonUpstreamTLS
	case errors.Is(err, context.DeadlineExceeded), errors.As(err, &ne) && ne.Timeout():
		return reasonUpstreamTimeout
	}
	return reasonUpstream
}

func ruleAllows(r *policy.HTTPRule, method, p string) bool {
	if len(r.Methods) > 0 && !slices.Contains(r.Methods, method) {
		return false
	}
	if len(r.Paths) == 0 {
		return true
	}
	return slices.ContainsFunc(r.Paths, func(pat string) bool {
		ok, _ := path.Match(pat, p)
		return ok
	})
}

// ambiguousPath reports whether u's target could name something other
// than its decoded path, which is what rules match. url.URL sets RawPath
// only when the target differs from the canonical escaping of Path, which
// is then what the transport forwards. A backslash is canonically %5C but
// some servers read it as a separator, and a repeated slash, which a rule
// matches as an empty segment, many servers collapse. Opaque and
// asterisk-form targets have no origin-form path at all.
func ambiguousPath(u *url.URL) bool {
	return u.RawPath != "" || u.Opaque != "" || !strings.HasPrefix(u.Path, "/") || strings.Contains(u.Path, `\`) || strings.Contains(u.Path, "//")
}

func hasDotSegment(p string) bool {
	return slices.ContainsFunc(strings.Split(p, "/"), func(s string) bool { return s == "." || s == ".." })
}

// unlisted is a placeholder no matched rule lists and the one header that
// carries it.
type unlisted struct{ name, header string }

// needed returns the credentials r's headers call for: each placeholder it
// carries, and each allowed credential whose inject header is missing.
// Headers are read by name, then value, then position. With brokered, the
// placeholders outside allowed, which only the broker can resolve, come
// back separately in that order; without it the first one is a refusal. A
// placeholder in a reserved header, or one outside allowed in two headers,
// is a refusal.
func needed(h http.Header, allowed []policy.Credential, brokered bool) ([]policy.Credential, []unlisted, string) {
	byName := map[string]policy.Credential{}
	for _, c := range allowed {
		byName[c.Name] = c
	}
	var need []policy.Credential
	var unknown []unlisted
	seen := map[string]string{}
	add := func(c policy.Credential) {
		if !slices.ContainsFunc(need, func(n policy.Credential) bool { return n.Name == c.Name }) {
			need = append(need, c)
		}
	}
	for _, k := range slices.Sorted(maps.Keys(h)) {
		for _, v := range h[k] {
			names := placeholderNames(v)
			if inner, ok := basicPayload(k, v); ok {
				names = append(names, placeholderNames(inner)...)
			}
			if len(names) > 0 && policy.ReservedHeader(k) {
				return nil, nil, reasonPlaceholderHdr
			}
			for _, n := range names {
				switch c, ok := byName[n]; {
				case ok:
					add(c)
				case !brokered:
					return nil, nil, reasonPlaceholderOther
				case seen[n] == "":
					seen[n] = k
					unknown = append(unknown, unlisted{n, k})
				case seen[n] != k:
					return nil, nil, reasonPlaceholderElsewhere
				}
			}
		}
	}
	for _, c := range allowed {
		if _, ok := h[c.Inject.Header]; !ok {
			add(c)
		}
	}
	return need, unknown, ""
}

// apply swaps placeholders in h for values and sets missing inject
// headers. It returns the headers it wrote, which the outgoing request must
// carry as they are, and what to scrub from the response's headers.
func apply(h http.Header, need []policy.Credential, values map[string]string) (http.Header, scrubber) {
	set := http.Header{}
	scrub := scrubber{}
	for _, c := range need {
		scrub[values[c.Name]] = c.Placeholder()
	}
	for k, vs := range h {
		for i, v := range vs {
			if inner, ok := basicPayload(k, v); ok && strings.Contains(inner, policy.PlaceholderPrefix) {
				// A Basic payload echoed back is the encoded form.
				enc := base64.StdEncoding.EncodeToString([]byte(replacePlaceholders(inner, values)))
				scrub[enc] = strings.TrimPrefix(v, "Basic ")
				vs[i], set[k] = "Basic "+enc, vs
			} else if strings.Contains(v, policy.PlaceholderPrefix) {
				vs[i], set[k] = replacePlaceholders(v, values), vs
			}
		}
	}
	for _, c := range need {
		if _, ok := h[c.Inject.Header]; !ok {
			h.Set(c.Inject.Header, strings.Replace(c.Inject.Format, "{secret}", values[c.Name], 1))
			set[c.Inject.Header] = h[c.Inject.Header]
		}
	}
	return set, scrub
}

func (f *decryptFlow) log(l *reqLog) {
	attrs := []slog.Attr{
		slog.String("src", f.src.String()),
		slog.String("node", f.caller.Node),
		slog.Any("tags", f.caller.Tags),
		slog.String("host", f.v.Name),
		slog.String("method", l.method),
		// The path is the tenant's, but scrub it so no value is logged.
		slog.String("path", l.scrub.string(l.path)),
		slog.Int("status", l.status),
	}
	if len(l.rules) > 0 {
		attrs = append(attrs, slog.String("rule", strings.Join(l.rules, ",")))
	}
	if len(l.creds) > 0 {
		attrs = append(attrs, slog.Any("credentials", l.creds))
	}
	if len(l.broker) > 0 {
		attrs = append(attrs, slog.Any("broker", l.broker))
	}
	if l.reason != "" {
		attrs = append(attrs, slog.String("reason", l.reason))
	}
	if l.err != nil {
		attrs = append(attrs, slog.String("error", l.err.Error()))
	}
	f.d.Audit.LogAttrs(context.Background(), slog.LevelInfo, "request", attrs...)
}

// basicPayload decodes an Authorization: Basic value. Adapted from
// iron-proxy's decodeBasicAuth (internal/transform/secrets/secrets.go,
// Apache-2.0, see NOTICE).
func basicPayload(header, value string) (string, bool) {
	if header != "Authorization" {
		return "", false
	}
	enc, ok := strings.CutPrefix(value, "Basic ")
	if !ok {
		return "", false
	}
	dec, err := base64.StdEncoding.DecodeString(enc)
	if err != nil {
		return "", false
	}
	return string(dec), true
}

func isNameByte(b byte) bool {
	return b >= 'A' && b <= 'Z' || b >= '0' && b <= '9' || b == '_'
}

// placeholderNames returns the credential name of every placeholder in s.
func placeholderNames(s string) []string {
	var out []string
	for {
		i := strings.Index(s, policy.PlaceholderPrefix)
		if i < 0 {
			return out
		}
		s = s[i+len(policy.PlaceholderPrefix):]
		n := 0
		for n < len(s) && isNameByte(s[n]) {
			n++
		}
		out, s = append(out, s[:n]), s[n:]
	}
}

// replacePlaceholders swaps each placeholder in s whose name is in values.
func replacePlaceholders(s string, values map[string]string) string {
	var b strings.Builder
	for {
		i := strings.Index(s, policy.PlaceholderPrefix)
		if i < 0 {
			b.WriteString(s)
			return b.String()
		}
		b.WriteString(s[:i])
		rest := s[i+len(policy.PlaceholderPrefix):]
		n := 0
		for n < len(rest) && isNameByte(rest[n]) {
			n++
		}
		if v, ok := values[rest[:n]]; ok {
			b.WriteString(v)
		} else {
			b.WriteString(s[i : i+len(policy.PlaceholderPrefix)+n])
		}
		s = rest[n:]
	}
}

// serveOne runs an HTTP/1.1 server on conn until the connection closes,
// or ends in an upgrade whose handler returns, or ctx ends.
func serveOne(ctx context.Context, conn net.Conn, h http.Handler) {
	var handlers sync.WaitGroup
	done := make(chan struct{})
	var once sync.Once
	srv := &http.Server{
		Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			handlers.Add(1)
			defer handlers.Done()
			h.ServeHTTP(w, r)
		}),
		ReadHeaderTimeout: 30 * time.Second,
		IdleTimeout:       2 * time.Minute,
		TLSNextProto:      map[string]func(*http.Server, *tls.Conn, http.Handler){},
		ErrorLog:          slog.NewLogLogger(slog.Default().Handler(), slog.LevelDebug),
		BaseContext:       func(net.Listener) context.Context { return ctx },
		ConnState: func(_ net.Conn, s http.ConnState) {
			if s == http.StateClosed || s == http.StateHijacked {
				once.Do(func() { close(done) })
			}
		},
	}
	ln := &oneConnListener{conn: conn, closed: make(chan struct{})}
	served := make(chan struct{})
	go func() {
		_ = srv.Serve(ln)
		close(served)
	}()
	select {
	case <-done:
	case <-ctx.Done():
		// A request read before the close may still reach its handler.
		_ = conn.Close()
		<-done
	}
	_ = srv.Close()
	<-served
	// The connection is closed or hijacked and Serve has returned, so no
	// handler starts after Wait does.
	handlers.Wait()
}

type oneConnListener struct {
	conn   net.Conn
	used   atomic.Bool
	once   sync.Once
	closed chan struct{}
}

func (l *oneConnListener) Accept() (net.Conn, error) {
	if l.used.CompareAndSwap(false, true) {
		return l.conn, nil
	}
	<-l.closed
	return nil, net.ErrClosed
}

func (l *oneConnListener) Close() error {
	l.once.Do(func() { close(l.closed) })
	return nil
}

func (l *oneConnListener) Addr() net.Addr { return l.conn.LocalAddr() }

// countingConn replays the peeked bytes and counts traffic both ways.
type countingConn struct {
	net.Conn
	r       io.Reader
	in, out atomic.Int64
}

func (c *countingConn) Read(p []byte) (int, error) {
	n, err := c.r.Read(p)
	c.in.Add(int64(n))
	return n, err
}

func (c *countingConn) Write(p []byte) (int, error) {
	n, err := c.Conn.Write(p)
	c.out.Add(int64(n))
	return n, err
}
