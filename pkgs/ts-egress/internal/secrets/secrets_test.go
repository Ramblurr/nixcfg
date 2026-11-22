package secrets

import (
	"context"
	"encoding/json"
	"errors"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync/atomic"
	"testing"
	"time"

	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/policy"
)

type fakeOP struct {
	calls atomic.Int32
	value string
	err   error
}

func (f *fakeOP) Fetch(context.Context, policy.OPRef) (string, error) {
	f.calls.Add(1)
	return f.value, f.err
}

func opRef(t *testing.T, s string) policy.SecretRef {
	t.Helper()
	ref, err := policy.ParseOPRef(s)
	if err != nil {
		t.Fatal(err)
	}
	return policy.SecretRef{Scheme: policy.SecretOnePassword, Ref: s, OP: ref}
}

func TestLiteral(t *testing.T) {
	var s Store
	v, err := s.Get(context.Background(), policy.SecretRef{Scheme: policy.SecretLiteral, Ref: "egress-dummy-value"})
	if err != nil || v != "egress-dummy-value" {
		t.Fatalf("got %q %v", v, err)
	}
	if _, err := s.Get(context.Background(), opRef(t, "op://v/i/f")); !errors.Is(err, ErrNotConfigured) {
		t.Fatalf("op without connect: %v", err)
	}
}

// A value is cached for the TTL. After it, a failed refresh is an error,
// not the stale value, and the error does not carry the value.
func TestCacheFailsClosedAfterExpiry(t *testing.T) {
	now := time.Unix(0, 0)
	op := &fakeOP{value: "real-1"}
	s := &Store{OP: op, TTL: time.Minute, now: func() time.Time { return now }}
	ref := opRef(t, "op://LLM/github-test/credential")
	get := func() (string, error) { return s.Get(context.Background(), ref) }

	if v, err := get(); v != "real-1" || err != nil {
		t.Fatalf("first get = %q %v", v, err)
	}
	op.err = errors.New("connect answered 503 Service Unavailable")
	now = now.Add(59 * time.Second)
	if v, err := get(); v != "real-1" || err != nil || op.calls.Load() != 1 {
		t.Fatalf("cached get = %q %v after %d fetches", v, err, op.calls.Load())
	}
	now = now.Add(2 * time.Second)
	v, err := get()
	if v != "" || err == nil || strings.Contains(err.Error(), "real-1") {
		t.Fatalf("expired get with a failing refresh = %q %v", v, err)
	}
	op.err, op.value = nil, "real-2"
	if v, err := get(); v != "real-2" || err != nil {
		t.Fatalf("recovered get = %q %v", v, err)
	}
	op.value = ""
	now = now.Add(2 * time.Minute)
	if v, err := get(); v != "" || err == nil {
		t.Fatalf("empty value = %q %v", v, err)
	}
}

// net/http trims a header value's surrounding whitespace and refuses
// control bytes, so such a value would reach the upstream as other bytes
// than the scrubber looks for. It is refused, and not served.
func TestRejectsValuesTheWireWouldChange(t *testing.T) {
	for _, bad := range []string{"tok\r\n", "tok\nX-Other: y", " tok", "tok\t", "to\x00k", "to\x7fk"} {
		op := &fakeOP{value: bad}
		s := &Store{OP: op}
		v, err := s.Get(context.Background(), opRef(t, "op://LLM/github-test/credential"))
		if v != "" || err == nil || strings.Contains(err.Error(), "tok") {
			t.Fatalf("%q: got %q %v", bad, v, err)
		}
	}
}

func TestParseOPRef(t *testing.T) {
	for in, want := range map[string]policy.OPRef{
		"op://LLM/github-test/credential":   {Vault: "LLM", Item: "github-test", Field: "credential"},
		"op://LLM/github-test/api/password": {Vault: "LLM", Item: "github-test", Section: "api", Field: "password"},
	} {
		if got, err := policy.ParseOPRef(in); err != nil || got != want {
			t.Fatalf("%s = %+v %v", in, got, err)
		}
	}
	for _, bad := range []string{"op://LLM/item", "op://LLM//field", "op://a/b/c/d/e", "literal:x", "op:/a/b/c"} {
		if _, err := policy.ParseOPRef(bad); err == nil {
			t.Fatalf("%s parsed", bad)
		}
	}
}

// fakeConnect serves the parts of the Connect REST API that Connect uses.
func fakeConnect(t *testing.T) *httptest.Server {
	t.Helper()
	const vault, item = "vaultidvaultidvaultidvault", "itemiditemiditemiditemidit"
	mux := http.NewServeMux()
	write := func(w http.ResponseWriter, v any) { _ = json.NewEncoder(w).Encode(v) }
	auth := func(h http.HandlerFunc) http.HandlerFunc {
		return func(w http.ResponseWriter, r *http.Request) {
			if r.Header.Get("Authorization") != "Bearer connect-token" {
				w.WriteHeader(http.StatusUnauthorized)
				return
			}
			h(w, r)
		}
	}
	mux.HandleFunc("GET /v1/vaults", auth(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Query().Get("filter") != `title eq "LLM"` {
			write(w, []any{})
			return
		}
		write(w, []map[string]string{{"id": vault}})
	}))
	mux.HandleFunc("GET /v1/vaults/"+vault+"/items", auth(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Query().Get("filter") != `title eq "github-test"` {
			write(w, []any{})
			return
		}
		write(w, []map[string]string{{"id": item}})
	}))
	mux.HandleFunc("GET /v1/vaults/"+vault+"/items/"+item, auth(func(w http.ResponseWriter, _ *http.Request) {
		write(w, map[string]any{
			"sections": []map[string]string{
				{"id": "sec1", "label": "api"},
				{"id": "sec2", "label": "sec1"},
				{"id": "sec3", "label": "dup"},
				{"id": "sec4", "label": "dup"},
			},
			"fields": []map[string]any{
				{"id": "password", "label": "password", "value": "top-level"},
				{"id": "credential", "label": "credential", "value": "ghp_from_connect"},
				{"id": "f2", "label": "password", "value": "in-section", "section": map[string]string{"id": "sec1"}},
				{"id": "f3", "label": "apikey", "value": "by-label"},
				{"id": "apikey", "label": "other", "value": "by-id"},
				{"id": "f5", "label": "token", "value": "token-top"},
				{"id": "f6", "label": "token", "value": "token-api", "section": map[string]string{"id": "sec1"}},
				{"id": "f7", "label": "password", "value": "shadow", "section": map[string]string{"id": "sec2"}},
				{"id": "f8", "label": "only", "value": "in-dup", "section": map[string]string{"id": "sec3"}},
			},
		})
	}))
	srv := httptest.NewServer(mux)
	t.Cleanup(srv.Close)
	return srv
}

func TestConnect(t *testing.T) {
	srv := fakeConnect(t)
	c := &Connect{Host: srv.URL + "/", Token: "connect-token"}
	for ref, want := range map[string]string{
		"op://LLM/github-test/credential":    "ghp_from_connect",
		"op://LLM/github-test/password":      "top-level",
		"op://LLM/github-test/api/password":  "in-section",
		"op://LLM/github-test/sec1/f2":       "in-section",
		"op://LLM/github-test/sec1/password": "in-section",
		"op://LLM/github-test/sec3/only":     "in-dup",
		"op://LLM/github-test/apikey":        "by-id",
		"op://LLM/github-test/api/token":     "token-api",
	} {
		r, _ := policy.ParseOPRef(ref)
		if got, err := c.Fetch(context.Background(), r); err != nil || got != want {
			t.Fatalf("%s = %q %v", ref, got, err)
		}
	}
	for _, ref := range []string{"op://Other/github-test/credential", "op://LLM/other/credential", "op://LLM/github-test/missing", "op://LLM/github-test/nosec/password", "op://LLM/github-test/token", "op://LLM/github-test/dup/only", "op://LLM/github-test/sec1/f7"} {
		r, _ := policy.ParseOPRef(ref)
		if got, err := c.Fetch(context.Background(), r); err == nil {
			t.Fatalf("%s = %q", ref, got)
		}
	}
	bad := &Connect{Host: srv.URL, Token: "wrong"}
	r, _ := policy.ParseOPRef("op://LLM/github-test/credential")
	if _, err := bad.Fetch(context.Background(), r); err == nil || !strings.Contains(err.Error(), "401") {
		t.Fatalf("wrong token: %v", err)
	}
}

// Connect's answers are the server's bytes; none of them reach an error,
// which ends up in the audit log.
func TestConnectErrorsCarryNoResponseBytes(t *testing.T) {
	const marker = "LEAKMARK"
	for name, raw := range map[string]string{
		"reason phrase": "HTTP/1.1 503 " + marker + "\r\nContent-Length: 0\r\n\r\n",
		"bad json":      "HTTP/1.1 200 OK\r\nContent-Length: 12\r\n\r\n[{\"id\":" + marker[:1] + "}]   ",
		"bad framing":   "HTTP/1.1 200 OK\r\n" + marker + "\r\n\r\n",
		"bad status":    marker + " 200 OK\r\n\r\n",
	} {
		t.Run(name, func(t *testing.T) {
			srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
				c, _, err := http.NewResponseController(w).Hijack()
				if err != nil {
					return
				}
				defer c.Close()
				_, _ = io.WriteString(c, raw)
			}))
			t.Cleanup(srv.Close)
			c := &Connect{Host: srv.URL, Token: "connect-token"}
			r, _ := policy.ParseOPRef("op://LLM/github-test/credential")
			_, err := c.Fetch(context.Background(), r)
			if err == nil || strings.Contains(err.Error(), marker) || strings.Contains(err.Error(), "invalid character") {
				t.Fatalf("err = %v", err)
			}
		})
	}
}
