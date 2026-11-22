package egress

import (
	"bufio"
	"context"
	"encoding/json"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"slices"
	"sync"
	"testing"

	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/broker"
	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/policy"
	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/secrets"
)

// fakeBroker answers decide requests on a unix socket with the line for
// the credential asked about and records each request it read.
type fakeBroker struct {
	socket string
	answer func(credential string) string

	mu   sync.Mutex
	reqs []map[string]any
}

func newFakeBroker(t *testing.T, answer string) *fakeBroker {
	t.Helper()
	return serveFakeBroker(t, func(string) string { return answer })
}

func newFakeBrokerFor(t *testing.T, answers map[string]string) *fakeBroker {
	t.Helper()
	return serveFakeBroker(t, func(credential string) string { return answers[credential] })
}

func serveFakeBroker(t *testing.T, answer func(string) string) *fakeBroker {
	t.Helper()
	// A unix socket path must stay short, which t.TempDir's may not.
	dir, err := os.MkdirTemp("", "brk")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { os.RemoveAll(dir) })
	b := &fakeBroker{socket: filepath.Join(dir, "decide.sock"), answer: answer}
	ln, err := net.Listen("unix", b.socket)
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
			go func() {
				defer c.Close()
				line, err := bufio.NewReader(c).ReadBytes('\n')
				if err != nil {
					return
				}
				var req map[string]any
				_ = json.Unmarshal(line, &req)
				b.mu.Lock()
				b.reqs = append(b.reqs, req)
				b.mu.Unlock()
				credential, _ := req["credential"].(string)
				_, _ = c.Write([]byte(b.answer(credential) + "\n"))
			}()
		}
	}()
	return b
}

func (b *fakeBroker) requests() []map[string]any {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.reqs
}

// recordingSecrets answers every op:// reference with ghSecret and keeps
// the references it was asked for.
type recordingSecrets struct {
	mu   sync.Mutex
	refs []policy.OPRef
}

func (s *recordingSecrets) Get(_ context.Context, ref policy.SecretRef) (string, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.refs = append(s.refs, ref.OP)
	return ghSecret, nil
}

const allowGitHub = `{"decision":"allow","approval":"ap-7",` +
	`"secret":{"vault":"v1","item":"i1","field":"credential"},` +
	`"inject":{"header":"Authorization"}}`

func allowInto(header string) string {
	return `{"decision":"allow","approval":null,"secret":{"vault":"v1","item":"i1","field":"credential"},` +
		`"inject":{"header":"` + header + `"}}`
}

func openRule() []*policy.HTTPRule {
	return []*policy.HTTPRule{{ID: "g0.http0", Ports: []uint16{443}}}
}

func withBroker(socket string) func(*HTTPDecrypter) {
	return func(d *HTTPDecrypter) { d.Broker = &broker.Client{Socket: socket} }
}

// brokerCaller is a tenant whose caps let the broker decide for it.
func brokerCaller(tags ...string) *Caller {
	return &Caller{Node: "t3-alice", Tags: tags, Policy: policy.Policy{
		Broker: &policy.BrokerCeiling{Groups: []string{"agents"}, Modes: []policy.BrokerMode{policy.BrokerInject}},
	}}
}

func ghAuth() map[string]string {
	return map[string]string{"Authorization": "Bearer " + ghCred.Placeholder()}
}

// brokerLines returns the broker field of each request audit line.
func brokerLines(t *testing.T, h *harness) []any {
	t.Helper()
	var out []any
	for _, l := range h.requests(t) {
		out = append(out, l["broker"])
	}
	return out
}

// A placeholder no rule lists is injected with the credential the broker
// names, fetched from 1Password and scrubbed from the response like a
// static one. The broker is told who asks, for what and where.
func TestBrokerAllowInjects(t *testing.T) {
	b := newFakeBroker(t, allowGitHub)
	sec := &recordingSecrets{}
	h := newHarness(t, true, openRule(), sec, echo, withBroker(b.socket))
	h.setCaller(brokerCaller("tag:t3-zed", "tag:t3-alice"))

	resp, body := h.response(t, "GET", "https://example.com/user", ghAuth())
	if resp.StatusCode != 200 {
		t.Fatalf("status %d: %s", resp.StatusCode, body)
	}
	if got := h.upstream.all()[0].Header.Get("Authorization"); got != "Bearer "+ghSecret {
		t.Fatalf("upstream Authorization = %q", got)
	}
	if got := resp.Header.Get("X-Echo-Authorization"); got != "Bearer "+ghCred.Placeholder() {
		t.Fatalf("echoed Authorization = %q", got)
	}
	if len(sec.refs) != 1 || sec.refs[0] != (policy.OPRef{Vault: "v1", Item: "i1", Field: "credential", FieldByID: true}) {
		t.Fatalf("secret refs = %+v", sec.refs)
	}
	want := `[{"credential":"GITHUB_TOKEN","groups":["agents"],"header":"Authorization","host":"example.com","identity":"tag:t3-alice,tag:t3-zed",` +
		`"mode":"inject","modes":["inject"],"node":"t3-alice","op":"decide"}]`
	if got := toJSON(b.requests()); got != want {
		t.Fatalf("broker got %s\nwant %s", got, want)
	}
	l := h.requests(t)
	if len(l) != 1 || toJSON(l[0]["credentials"]) != `["GITHUB_TOKEN"]` ||
		toJSON(l[0]["broker"]) != `[{"approval":"ap-7","credential":"GITHUB_TOKEN","decision":"allow"}]` {
		t.Fatalf("audit = %v", l)
	}
}

// An untagged caller is known to the broker by its login.
func TestBrokerIdentifiesUntaggedCallersByLogin(t *testing.T) {
	b := newFakeBroker(t, allowGitHub)
	h := newHarness(t, true, openRule(), fakeSecrets{}, echo, withBroker(b.socket))
	c := brokerCaller()
	c.Login = "alice@example.com"
	h.setCaller(c)
	if code, body := h.do(t, "GET", "https://example.com/user", ghAuth()); code != 200 {
		t.Fatalf("status %d: %s", code, body)
	}
	if r := b.requests(); len(r) != 1 || r[0]["identity"] != "alice@example.com" {
		t.Fatalf("broker got %v", r)
	}
}

func TestBrokerPendingAsksForApproval(t *testing.T) {
	b := newFakeBroker(t, `{"decision":"pending","approval":"ap-9"}`)
	h := newHarness(t, true, openRule(), fakeSecrets{}, echo, withBroker(b.socket))
	h.setCaller(brokerCaller("tag:t3-alice"))
	code, body := h.do(t, "GET", "https://example.com/user", ghAuth())
	if code != http.StatusForbidden || body != "approval-pending ap-9\n" {
		t.Fatalf("status %d: %q", code, body)
	}
	if n := len(h.upstream.all()); n != 0 {
		t.Fatalf("upstream got %d requests", n)
	}
	if l := h.requests(t); len(l) != 1 || l[0]["reason"] != reasonApprovalPending ||
		toJSON(l[0]["broker"]) != `[{"approval":"ap-9","credential":"GITHUB_TOKEN","decision":"pending"}]` {
		t.Fatalf("audit = %v", l)
	}
}

// A denial, an unreachable broker and an answer the egress cannot use all
// refuse the request as an unknown placeholder, with the cause logged.
func TestBrokerFailsClosed(t *testing.T) {
	for name, tt := range map[string]struct {
		answer string // empty: no broker listens
		reason string
	}{
		"deny":            {`{"decision":"deny","reason":"unassigned"}`, "unassigned"},
		"socket down":     {"", reasonBrokerUnavailable},
		"not json":        {`allow`, reasonBrokerUnavailable},
		"allow no secret": {`{"decision":"allow","approval":null,"inject":{"header":"Authorization"}}`, reasonBrokerUnavailable},
		"allow to host":   {allowInto("Host"), reasonBrokerUnavailable},
		"allow to cookie": {allowInto("Cookie"), reasonBrokerUnavailable},
		"allow to expect": {allowInto("expect"), reasonBrokerUnavailable},
		"unknown":         {`{"decision":"maybe"}`, reasonBrokerUnavailable},
	} {
		t.Run(name, func(t *testing.T) {
			socket := filepath.Join(t.TempDir(), "none.sock")
			if tt.answer != "" {
				socket = newFakeBroker(t, tt.answer).socket
			}
			h := newHarness(t, true, openRule(), fakeSecrets{}, echo, withBroker(socket))
			h.setCaller(brokerCaller("tag:t3-alice"))
			code, body := h.do(t, "GET", "https://example.com/user", ghAuth())
			if code != http.StatusForbidden || body != "ts-egress: "+reasonPlaceholderOther+"\n" {
				t.Fatalf("status %d: %q", code, body)
			}
			if n := len(h.upstream.all()); n != 0 {
				t.Fatalf("upstream got %d requests", n)
			}
			l := h.requests(t)
			want := `[{"credential":"GITHUB_TOKEN","decision":"deny","reason":"` + tt.reason + `"}]`
			if len(l) != 1 || l[0]["reason"] != reasonPlaceholderOther || toJSON(l[0]["broker"]) != want {
				t.Fatalf("audit = %v", l)
			}
		})
	}
}

// The broker is asked only for a placeholder no rule lists, on a decrypted
// TLS flow, for a caller whose caps carry a broker ceiling with inject.
func TestBrokerNotAsked(t *testing.T) {
	for name, tt := range map[string]struct {
		tls    bool
		rules  []*policy.HTTPRule
		caller *Caller
		code   int
	}{
		"no ceiling":        {true, openRule(), &Caller{Node: "t3-alice", Tags: []string{"tag:t3-alice"}}, 403},
		"ceiling no modes":  {true, openRule(), &Caller{Node: "t3-alice", Tags: []string{"tag:t3-alice"}, Policy: policy.Policy{Broker: &policy.BrokerCeiling{Groups: []string{"agents"}}}}, 403},
		"no identity":       {true, openRule(), brokerCaller(), 403},
		"static credential": {true, []*policy.HTTPRule{{ID: "g0.http0", Ports: []uint16{443}, Credentials: []policy.Credential{ghCred}}}, brokerCaller("tag:t3-alice"), 200},
		"plain http":        {false, []*policy.HTTPRule{{ID: "g0.http0", Ports: []uint16{80}}}, brokerCaller("tag:t3-alice"), 403},
	} {
		t.Run(name, func(t *testing.T) {
			b := newFakeBroker(t, allowGitHub)
			h := newHarness(t, tt.tls, tt.rules, fakeSecrets{}, echo, withBroker(b.socket))
			h.setCaller(tt.caller)
			scheme := "https"
			if !tt.tls {
				scheme = "http"
			}
			if code, body := h.do(t, "GET", scheme+"://example.com/user", ghAuth()); code != tt.code {
				t.Fatalf("status %d: %s", code, body)
			}
			if n := len(b.requests()); n != 0 {
				t.Fatalf("broker asked %d times", n)
			}
			if l := brokerLines(t, h); len(l) != 1 || l[0] != nil {
				t.Fatalf("broker audit = %v", l)
			}
		})
	}
}

// A decision is never reused: every request asks again.
func TestBrokerAskedOnEveryRequest(t *testing.T) {
	b := newFakeBroker(t, allowGitHub)
	h := newHarness(t, true, openRule(), fakeSecrets{}, echo, withBroker(b.socket))
	h.setCaller(brokerCaller("tag:t3-alice"))
	for range 3 {
		if code, body := h.do(t, "GET", "https://example.com/user", ghAuth()); code != 200 {
			t.Fatalf("status %d: %s", code, body)
		}
	}
	if n := len(b.requests()); n != 3 {
		t.Fatalf("broker asked %d times, want 3", n)
	}
}

// The broker is told the canonical name of the header that carries the
// placeholder, so it can refuse before an approval is spent.
func TestBrokerToldTheHeader(t *testing.T) {
	b := newFakeBroker(t, allowInto("X-Api-Key"))
	h := newHarness(t, true, openRule(), fakeSecrets{}, echo, withBroker(b.socket))
	h.setCaller(brokerCaller("tag:t3-alice"))
	if code, body := h.do(t, "GET", "https://example.com/user", map[string]string{"x-api-key": ghCred.Placeholder()}); code != 200 {
		t.Fatalf("status %d: %s", code, body)
	}
	if r := b.requests(); len(r) != 1 || r[0]["header"] != "X-Api-Key" {
		t.Fatalf("broker got %v", r)
	}
}

// A broker placeholder is honoured only in the header the broker approved
// it for. Anywhere else the request is refused before a secret is read,
// even when the broker allows it.
func TestBrokerPlaceholderStaysInItsHeader(t *testing.T) {
	p := ghCred.Placeholder()
	for name, tt := range map[string]struct {
		hdr  map[string]string
		asks []string
	}{
		"also in another header": {map[string]string{"Authorization": "Bearer " + p, "X-Other": p}, nil},
		"only in another header": {map[string]string{"X-Other": p}, []string{"X-Other"}},
	} {
		t.Run(name, func(t *testing.T) {
			b := newFakeBroker(t, allowGitHub)
			sec := &recordingSecrets{}
			h := newHarness(t, true, openRule(), sec, echo, withBroker(b.socket))
			h.setCaller(brokerCaller("tag:t3-alice"))
			if code, body := h.do(t, "GET", "https://example.com/user", tt.hdr); code != http.StatusForbidden {
				t.Fatalf("status %d: %s", code, body)
			}
			if len(sec.refs) != 0 || len(h.upstream.all()) != 0 {
				t.Fatalf("secrets read %v, upstream got %d requests", sec.refs, len(h.upstream.all()))
			}
			var asks []string
			for _, r := range b.requests() {
				hdr, _ := r["header"].(string)
				asks = append(asks, hdr)
			}
			if !slices.Equal(asks, tt.asks) {
				t.Fatalf("broker asked for headers %v, want %v", asks, tt.asks)
			}
			if l := h.requests(t); len(l) != 1 || l[0]["reason"] != reasonPlaceholderElsewhere {
				t.Fatalf("audit = %v", l)
			}
		})
	}
}

// A name the caller's caps list as a static credential, on any rule, is
// refused where the matched rule does not list it, never put to the broker.
func TestBrokerNeverAskedForAStaticName(t *testing.T) {
	b := newFakeBroker(t, allowGitHub)
	h := newHarness(t, true, openRule(), fakeSecrets{}, echo, withBroker(b.socket))
	c := brokerCaller("tag:t3-alice")
	other, err := policy.ParseHostPattern("api.github.com")
	if err != nil {
		t.Fatal(err)
	}
	c.Policy.HTTP = []policy.HTTPRule{{ID: "g1.http0", Hosts: []policy.HostPattern{other}, Ports: []uint16{443}, Credentials: []policy.Credential{ghCred}}}
	h.setCaller(c)
	if code, body := h.do(t, "GET", "https://example.com/user", ghAuth()); code != http.StatusForbidden {
		t.Fatalf("status %d: %s", code, body)
	}
	if n := len(b.requests()); n != 0 {
		t.Fatalf("broker asked %d times", n)
	}
	if l := h.requests(t); len(l) != 1 || l[0]["reason"] != reasonPlaceholderOther {
		t.Fatalf("audit = %v", l)
	}
}

// Placeholders are put to the broker by header name, then value, then
// position, whatever order the headers arrived in.
func TestBrokerDecidesInHeaderOrder(t *testing.T) {
	b := newFakeBrokerFor(t, map[string]string{"ZED_TOKEN": allowInto("X-Alpha"), "ABE_TOKEN": allowInto("X-Beta")})
	h := newHarness(t, true, openRule(), fakeSecrets{}, echo, withBroker(b.socket))
	h.setCaller(brokerCaller("tag:t3-alice"))
	hdr := map[string]string{"X-Beta": policy.PlaceholderPrefix + "ABE_TOKEN", "X-Alpha": policy.PlaceholderPrefix + "ZED_TOKEN"}
	const runs = 20
	for range runs {
		if code, body := h.do(t, "GET", "https://example.com/user", hdr); code != 200 {
			t.Fatalf("status %d: %s", code, body)
		}
	}
	reqs := b.requests()
	if len(reqs) != 2*runs {
		t.Fatalf("broker asked %d times", len(reqs))
	}
	for i, r := range reqs {
		if want := []string{"ZED_TOKEN", "ABE_TOKEN"}[i%2]; r["credential"] != want {
			t.Fatalf("decision %d was for %v, want %s", i, r["credential"], want)
		}
	}
}

// A caller the broker cannot decide for is refused at the first
// placeholder no rule lists, as before the broker existed.
func TestNoBrokerRefusesAtTheFirstUnlistedPlaceholder(t *testing.T) {
	b := newFakeBroker(t, allowGitHub)
	h := newHarness(t, true, openRule(), fakeSecrets{}, echo, withBroker(b.socket))
	code, body := h.do(t, "GET", "https://example.com/user", map[string]string{
		"A-Other":         policy.PlaceholderPrefix + "NOPE",
		"X-Forwarded-For": policy.PlaceholderPrefix + "ALSO_NOPE",
	})
	if code != http.StatusForbidden {
		t.Fatalf("status %d: %s", code, body)
	}
	if l := h.requests(t); len(l) != 1 || l[0]["reason"] != reasonPlaceholderOther {
		t.Fatalf("audit = %v", l)
	}
	if n := len(b.requests()); n != 0 {
		t.Fatalf("broker asked %d times", n)
	}
}

// fakeConnect is a 1Password Connect server holding one item whose only
// field has ID connectFieldID and label "credential".
func fakeConnect(t *testing.T) *secrets.Store {
	t.Helper()
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/v1/vaults/"+connectVault+"/items/"+connectItem {
			http.NotFound(w, r)
			return
		}
		_, _ = io.WriteString(w, `{"fields":[{"id":"`+connectFieldID+`","label":"credential","value":"`+ghSecret+`"}]}`)
	}))
	t.Cleanup(srv.Close)
	return &secrets.Store{OP: &secrets.Connect{Host: srv.URL, Token: "t"}}
}

const (
	connectVault   = "vvvvvvvvvvvvvvvvvvvvvvvvvv"
	connectItem    = "iiiiiiiiiiiiiiiiiiiiiiiiii"
	connectFieldID = "ffffffffffffffffffffffffff"
)

func allowField(field string) string {
	return `{"decision":"allow","approval":null,"secret":{"vault":"` + connectVault + `","item":"` + connectItem +
		`","field":"` + field + `"},"inject":{"header":"Authorization"}}`
}

// A broker credential names its field by ID. A label does not resolve it,
// not even when a static credential just read that field by its label.
func TestBrokerFieldResolvesByIDOnly(t *testing.T) {
	ref := "op://" + connectVault + "/" + connectItem + "/credential"
	op, err := policy.ParseOPRef(ref)
	if err != nil {
		t.Fatal(err)
	}
	static := policy.Credential{Name: "STATIC_TOKEN", Secret: policy.SecretRef{Scheme: policy.SecretOnePassword, Ref: ref, OP: op},
		Inject: policy.Inject{Header: "X-Static", Format: "{secret}"}}
	rules := []*policy.HTTPRule{
		{ID: "static", Ports: []uint16{443}, Paths: []string{"/static"}, Credentials: []policy.Credential{static}},
		{ID: "open", Ports: []uint16{443}, Paths: []string{"/user"}},
	}
	b := newFakeBrokerFor(t, map[string]string{"BY_LABEL": allowField("credential"), "BY_ID": allowField(connectFieldID)})
	h := newHarness(t, true, rules, fakeConnect(t), echo, withBroker(b.socket))
	h.setCaller(brokerCaller("tag:t3-alice"))
	bearer := func(name string) map[string]string {
		return map[string]string{"Authorization": "Bearer " + policy.PlaceholderPrefix + name}
	}
	for _, tt := range []struct {
		path string
		hdr  map[string]string
		code int
	}{
		{"/static", nil, 200},
		{"/user", bearer("BY_LABEL"), http.StatusBadGateway},
		{"/user", bearer("BY_ID"), 200},
		{"/user", bearer("BY_LABEL"), http.StatusBadGateway},
	} {
		if code, body := h.do(t, "GET", "https://example.com"+tt.path, tt.hdr); code != tt.code {
			t.Fatalf("%s %v: status %d, want %d: %s", tt.path, tt.hdr, code, tt.code, body)
		}
	}
}
