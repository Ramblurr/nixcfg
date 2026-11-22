package egress

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"net/url"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/policy"
	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/secrets"
)

const (
	contractToken  = "contract-connect-token"
	contractVault  = "vltcontract000000000000000"
	contractItem   = "itmcontract000000000000000"
	contractField  = "fldcontract000000000000000"
	contractSecret = "contract-secret-value"
	contractLogin  = "root@example"
)

// contractConnect serves one item. Its secret field has the label
// "credential"; another field has that label as its ID and a decoy value,
// so a broker that sent the label would inject the decoy.
func contractConnect(t *testing.T) string {
	t.Helper()
	item := map[string]any{
		"id": contractItem, "title": "GitHub token", "category": "API_CREDENTIAL",
		"vault": map[string]string{"id": contractVault},
		"fields": []map[string]string{
			{"id": contractField, "label": "credential", "type": "CONCEALED", "value": contractSecret},
			{"id": "credential", "label": "decoy", "type": "CONCEALED", "value": "decoy-value"},
		},
	}
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.Header.Get("Authorization") != "Bearer "+contractToken {
			http.Error(w, `{"status":401}`, http.StatusUnauthorized)
			return
		}
		var body any
		switch r.URL.Path {
		case "/v1/vaults":
			body = []map[string]string{{"id": contractVault, "name": "Agents"}}
		case "/v1/vaults/" + contractVault + "/items":
			body = []map[string]any{{"id": contractItem, "title": item["title"], "category": item["category"], "vault": item["vault"]}}
		case "/v1/vaults/" + contractVault + "/items/" + contractItem:
			body = item
		default:
			http.NotFound(w, r)
			return
		}
		w.Header().Set("Content-Type", "application/json")
		_ = json.NewEncoder(w).Encode(body)
	}))
	t.Cleanup(srv.Close)
	return srv.URL
}

type liveBroker struct {
	socket string
	ui     string
	cmd    *exec.Cmd
	out    *syncBuf
}

type syncBuf struct {
	mu  sync.Mutex
	buf bytes.Buffer
}

func (b *syncBuf) Write(p []byte) (int, error) {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.buf.Write(p)
}

func (b *syncBuf) String() string {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.buf.String()
}

func freePort(t *testing.T) int {
	t.Helper()
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	return ln.Addr().(*net.TCPAddr).Port
}

func startBroker(t *testing.T, connectURL string) *liveBroker {
	t.Helper()
	bin := os.Getenv("TS_BROKER")
	if bin == "" {
		t.Fatal("TS_BROKER must name the ts-broker program")
	}
	dir, err := os.MkdirTemp("", "brk")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { os.RemoveAll(dir) })
	token := filepath.Join(dir, "token")
	if err := os.WriteFile(token, []byte(contractToken+"\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	port := freePort(t)
	b := &liveBroker{socket: filepath.Join(dir, "decide.sock"), ui: fmt.Sprintf("http://127.0.0.1:%d", port), out: &syncBuf{}}
	b.cmd = exec.Command(bin, "serve", "--db", filepath.Join(dir, "broker.db"), "--socket", b.socket,
		"--port", fmt.Sprint(port), "--connect-url", connectURL, "--connect-token-file", token)
	b.cmd.Stdout, b.cmd.Stderr = b.out, b.out
	if err := b.cmd.Start(); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(b.stop)
	deadline := time.Now().Add(90 * time.Second)
	for {
		_, sockErr := os.Stat(b.socket)
		resp, httpErr := http.Get(b.ui + "/")
		if httpErr == nil {
			resp.Body.Close()
		}
		if sockErr == nil && httpErr == nil {
			return b
		}
		if time.Now().After(deadline) || b.cmd.ProcessState != nil {
			t.Fatalf("ts-broker did not start: %v %v\n%s", sockErr, httpErr, b.out)
		}
		time.Sleep(200 * time.Millisecond)
	}
}

func (b *liveBroker) stop() {
	if b.cmd.Process != nil && b.cmd.ProcessState == nil {
		_ = b.cmd.Process.Kill()
		_ = b.cmd.Wait()
	}
}

// post sends a UI action as the browser on the tailscale serve origin does.
func (b *liveBroker) post(t *testing.T, path string, form url.Values) {
	t.Helper()
	req, err := http.NewRequest("POST", b.ui+path, strings.NewReader(form.Encode()))
	if err != nil {
		t.Fatal(err)
	}
	req.Header.Set("Content-Type", "application/x-www-form-urlencoded")
	req.Header.Set("Tailscale-User-Login", contractLogin)
	req.Header.Set("Datastar-Request", "true")
	req.Header.Set("Sec-Fetch-Site", "same-origin")
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()
	body, _ := io.ReadAll(resp.Body)
	if resp.StatusCode != 200 {
		t.Fatalf("POST %s: %d %s", path, resp.StatusCode, body)
	}
}

func (b *liveBroker) credential(t *testing.T, name, hosts string) {
	t.Helper()
	b.post(t, "/action/create-credential", url.Values{
		"name": {name}, "vault": {contractVault}, "item": {contractItem}, "field": {contractField},
		"header": {"Authorization"}, "hosts": {hosts},
	})
}

func (b *liveBroker) assign(t *testing.T, credential, subject, approval string) {
	t.Helper()
	b.post(t, "/action/assign", url.Values{
		"credential": {credential}, "subject": {subject}, "mode": {"inject"}, "approval": {approval},
	})
}

func (b *liveBroker) approval(t *testing.T, id, event string) {
	t.Helper()
	b.post(t, "/action/approval", url.Values{"id": {id}, "event": {event}, "grant": {"once"}})
}

func contractCaller(groups []string, tags ...string) *Caller {
	return &Caller{Node: "t3-alice", Tags: tags, Policy: policy.Policy{
		Broker: &policy.BrokerCeiling{Groups: groups, Modes: []policy.BrokerMode{policy.BrokerInject}},
	}}
}

func TestBrokerContract(t *testing.T) {
	connect := contractConnect(t)
	b := startBroker(t, connect)
	store := &secrets.Store{OP: &secrets.Connect{Host: connect, Token: contractToken}}
	ok := func(w http.ResponseWriter, _ *http.Request) { _, _ = io.WriteString(w, "ok\n") }
	h := newHarness(t, true, openRule(), store, ok, withBroker(b.socket))

	b.credential(t, "GITHUB_TOKEN", "example.com")
	b.credential(t, "ELSEWHERE_TOKEN", "api.elsewhere.example")
	b.assign(t, "ELSEWHERE_TOKEN", "group:agents", "auto")

	type outcome struct {
		code     int
		body     string
		upstream string
		broker   string
	}
	seenUpstream := 0
	sendIn := func(t *testing.T, header, name, value string) outcome {
		t.Helper()
		code, body := h.do(t, "GET", "https://example.com/user",
			map[string]string{header: strings.Replace(value, "%p", policy.PlaceholderPrefix+name, 1)})
		o := outcome{code: code, body: body}
		if up := h.upstream.all(); len(up) > seenUpstream {
			o.upstream = up[len(up)-1].Header.Get("Authorization")
			seenUpstream = len(up)
		}
		lines := h.requests(t)
		o.broker = toJSON(lines[len(lines)-1]["broker"])
		h.audit.mu.Lock()
		leaked := strings.Contains(h.audit.buf.String(), contractSecret)
		h.audit.mu.Unlock()
		if leaked {
			t.Fatal("the egress audit holds the secret")
		}
		return o
	}
	send := func(t *testing.T, name, value string) outcome {
		t.Helper()
		return sendIn(t, "Authorization", name, value)
	}
	deny := func(name, reason string) outcome {
		return outcome{code: 403, body: "ts-egress: " + reasonPlaceholderOther + "\n",
			broker: `[{"credential":"` + name + `","decision":"deny","reason":"` + reason + `"}]`}
	}
	check := func(t *testing.T, got, want outcome) {
		t.Helper()
		if got != want {
			t.Fatalf("got  %+v\nwant %+v\nbroker output:\n%s", got, want, b.out)
		}
	}

	t.Run("deny reasons reach the egress audit", func(t *testing.T) {
		h.setCaller(contractCaller([]string{"agents"}, "tag:t3-alice"))
		check(t, send(t, "NO_SUCH_TOKEN", "Bearer %p"), deny("NO_SUCH_TOKEN", "unknown"))
		check(t, send(t, "ELSEWHERE_TOKEN", "Bearer %p"), deny("ELSEWHERE_TOKEN", "host"))
		check(t, send(t, "GITHUB_TOKEN", "Bearer %p"), deny("GITHUB_TOKEN", "unassigned"))
	})

	t.Run("an identity assignment matches the sorted tags", func(t *testing.T) {
		b.assign(t, "GITHUB_TOKEN", "tag:t3-alice,tag:t3-zed", "auto")
		h.setCaller(contractCaller([]string{"nobody"}, "tag:t3-zed", "tag:t3-alice"))
		check(t, send(t, "GITHUB_TOKEN", "token %p"), outcome{code: 200, body: "ok\n",
			upstream: "token " + contractSecret, broker: `[{"credential":"GITHUB_TOKEN","decision":"allow"}]`})
	})

	t.Run("a pending id is the one the UI approves and a wrong header does not spend it", func(t *testing.T) {
		b.assign(t, "GITHUB_TOKEN", "group:agents", "ask")
		h.setCaller(contractCaller([]string{"agents"}, "tag:t3-alice"))
		first := send(t, "GITHUB_TOKEN", "Bearer %p")
		id := strings.TrimSuffix(strings.TrimPrefix(first.body, "approval-pending "), "\n")
		check(t, first, outcome{code: 403, body: "approval-pending " + id + "\n",
			broker: `[{"approval":"` + id + `","credential":"GITHUB_TOKEN","decision":"pending"}]`})
		check(t, send(t, "GITHUB_TOKEN", "Bearer %p"), first)

		b.approval(t, id, "approve")
		check(t, sendIn(t, "X-Other", "GITHUB_TOKEN", "Bearer %p"), deny("GITHUB_TOKEN", "header"))
		check(t, send(t, "GITHUB_TOKEN", "Bearer %p"), outcome{code: 200, body: "ok\n", upstream: "Bearer " + contractSecret,
			broker: `[{"approval":"` + id + `","credential":"GITHUB_TOKEN","decision":"allow"}]`})

		again := send(t, "GITHUB_TOKEN", "Bearer %p")
		next := strings.TrimSuffix(strings.TrimPrefix(again.body, "approval-pending "), "\n")
		if next == id || again.code != 403 {
			t.Fatalf("the once approval %s was not spent: %+v", id, again)
		}
		b.approval(t, next, "block")
		check(t, send(t, "GITHUB_TOKEN", "Bearer %p"), deny("GITHUB_TOKEN", "blocked"))
	})

	t.Run("a stopped broker fails closed", func(t *testing.T) {
		h.setCaller(contractCaller([]string{"nobody"}, "tag:t3-zed", "tag:t3-alice"))
		b.stop()
		check(t, send(t, "GITHUB_TOKEN", "token %p"), deny("GITHUB_TOKEN", reasonBrokerUnavailable))
	})
}
