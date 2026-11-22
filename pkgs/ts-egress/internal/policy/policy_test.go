package policy

import (
	"errors"
	"net/netip"
	"os"
	"reflect"
	"slices"
	"strconv"
	"strings"
	"syscall"
	"testing"

	"tailscale.com/tailcfg"
)

func caps(t *testing.T, grants ...string) tailcfg.PeerCapMap {
	t.Helper()
	vals := make([]tailcfg.RawMessage, len(grants))
	for i, g := range grants {
		vals[i] = tailcfg.RawMessage(g)
	}
	return tailcfg.PeerCapMap{CapName: vals}
}

func file(t *testing.T, name string) string {
	t.Helper()
	b, err := os.ReadFile("testdata/" + name)
	if err != nil {
		t.Fatal(err)
	}
	return string(b)
}

func mustParse(t *testing.T, grants ...string) *Policy {
	t.Helper()
	p, err := Parse(caps(t, grants...))
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	return &p
}

func TestParseLiveCaps(t *testing.T) {
	p := mustParse(t, file(t, "alice.json"))
	if len(p.HTTP) != 2 || len(p.TLS) != 1 || len(p.TCP) != 1 || len(p.UDP) != 1 {
		t.Fatalf("rule counts: http=%d tls=%d tcp=%d udp=%d", len(p.HTTP), len(p.TLS), len(p.TCP), len(p.UDP))
	}
	if p.DNS.Allow != DNSRules || !p.Private.Block {
		t.Fatalf("dns=%q private.block=%v", p.DNS.Allow, p.Private.Block)
	}
	dummy := p.HTTP[1].Credentials[0]
	want := Credential{
		Name:   "DUMMY_TOKEN",
		Secret: SecretRef{Scheme: SecretLiteral, Ref: "egress-dummy-value"},
		Inject: Inject{Header: "X-Dummy-Token", Format: "{secret}"},
	}
	if !reflect.DeepEqual(dummy, want) {
		t.Fatalf("dummy credential = %+v, want %+v", dummy, want)
	}
	gh := p.HTTP[0].Credentials[0].Secret
	if gh != (SecretRef{Scheme: SecretOnePassword, Ref: "op://LLM/github-test/credential", OP: OPRef{Vault: "LLM", Item: "github-test", Field: "credential"}}) {
		t.Fatalf("github secret = %+v", gh)
	}
	mustParse(t, file(t, "bob.json"))
}

func TestParseRejects(t *testing.T) {
	for name, grant := range map[string]string{
		"unknown top field":     `{"tls":[{"hosts":["a.com"],"ports":[443]}],"allow":[]}`,
		"unknown nested field":  `{"tls":[{"hosts":["a.com"],"ports":[443],"sni":true}]}`,
		"unknown inject field":  `{"http":[{"hosts":["a.com"],"ports":[443],"credentials":[{"name":"T","secret":"literal:x","inject":{"header":"A","format":"{secret}","query":"q"}}]}]}`,
		"port out of range":     `{"tcp":[{"cidrs":["1.2.3.4/32"],"ports":[70000]}]}`,
		"port zero":             `{"tcp":[{"cidrs":["1.2.3.4/32"],"ports":[0]}]}`,
		"no ports":              `{"tls":[{"hosts":["a.com"]}]}`,
		"no hosts":              `{"tls":[{"ports":[443]}]}`,
		"l4 without target":     `{"udp":[{"ports":[123]}]}`,
		"bare ip cidr":          `{"udp":[{"cidrs":["1.2.3.4"],"ports":[123]}]}`,
		"bad cidr":              `{"deny":{"cidrs":["1.2.3.400/32"]}}`,
		"host with port":        `{"tls":[{"hosts":["a.com:443"],"ports":[443]}]}`,
		"non-ascii host":        `{"tls":[{"hosts":["bücher.de"],"ports":[443]}]}`,
		"trailing dot host":     `{"tls":[{"hosts":["a.com."],"ports":[443]}]}`,
		"leading dot host":      `{"tls":[{"hosts":[".a.com"],"ports":[443]}]}`,
		"empty label host":      `{"deny":{"hosts":["a..com"]}}`,
		"bad glob":              `{"tls":[{"hosts":["[a.com"],"ports":[443]}]}`,
		"slash in host":         `{"deny":{"hosts":["a/b.com"]}}`,
		"backslash in host":     `{"deny":{"hosts":["a\\b.com"]}}`,
		"control byte in host":  `{"deny":{"hosts":["a\u0001b.com"]}}`,
		"underscore mid-label":  `{"deny":{"hosts":["a_b.com"]}}`,
		"lower-case method":     `{"http":[{"hosts":["a.com"],"ports":[443],"methods":["get"]}]}`,
		"relative path":         `{"http":[{"hosts":["a.com"],"ports":[443],"paths":["v1/*"]}]}`,
		"plain secret":          `{"http":[{"hosts":["a.com"],"ports":[443],"credentials":[{"name":"T","secret":"hunter2","inject":{"header":"A","format":"{secret}"}}]}]}`,
		"short op ref":          `{"http":[{"hosts":["a.com"],"ports":[443],"credentials":[{"name":"T","secret":"op://vault/item","inject":{"header":"A","format":"{secret}"}}]}]}`,
		"no placeholder":        `{"http":[{"hosts":["a.com"],"ports":[443],"credentials":[{"name":"T","secret":"literal:x","inject":{"header":"A","format":"Bearer"}}]}]}`,
		"no inject":             `{"http":[{"hosts":["a.com"],"ports":[443],"credentials":[{"name":"T","secret":"literal:x"}]}]}`,
		"bad header":            `{"http":[{"hosts":["a.com"],"ports":[443],"credentials":[{"name":"T","secret":"literal:x","inject":{"header":"X Y","format":"{secret}"}}]}]}`,
		"format with crlf":      `{"http":[{"hosts":["a.com"],"ports":[443],"credentials":[{"name":"T","secret":"literal:x","inject":{"header":"A","format":"{secret}\r\nX-Other: y"}}]}]}`,
		"format with space":     `{"http":[{"hosts":["a.com"],"ports":[443],"credentials":[{"name":"T","secret":"literal:x","inject":{"header":"A","format":" Bearer {secret}"}}]}]}`,
		"literal with space":    `{"http":[{"hosts":["a.com"],"ports":[443],"credentials":[{"name":"T","secret":"literal:x ","inject":{"header":"A","format":"{secret}"}}]}]}`,
		"literal with newline":  `{"http":[{"hosts":["a.com"],"ports":[443],"credentials":[{"name":"T","secret":"literal:x\ny","inject":{"header":"A","format":"{secret}"}}]}]}`,
		"dns allow other":       `{"dns":{"allow":"all"}}`,
		"wrong type":            `{"tls":{"hosts":["a.com"]}}`,
		"not an object":         `["tls"]`,
		"v4-mapped cidr":        `{"tcp":[{"cidrs":["::ffff:203.0.113.0/120"],"ports":[22]}]}`,
		"v4-mapped deny cidr":   `{"deny":{"cidrs":["::ffff:198.51.100.0/120"]}}`,
		"duplicate key":         `{"tls":[{"hosts":["a.com"],"ports":[443]}],"tls":[]}`,
		"duplicate deny null":   `{"deny":{"hosts":["bad.com"]},"deny":null}`,
		"duplicate nested key":  `{"tls":[{"hosts":["a.com"],"ports":[443],"ports":[8443]}]}`,
		"duplicate folded key":  `{"deny":{"hosts":["bad.com"]},"Deny":null}`,
		"broker sign mode":      `{"broker":{"groups":["agents"],"modes":["sign"]}}`,
		"broker empty group":    `{"broker":{"groups":[""],"modes":["inject"]}}`,
		"broker prefixed group": `{"broker":{"groups":["group:agents"],"modes":["inject"]}}`,
		"broker unknown key":    `{"broker":{"groups":["agents"],"modes":["inject"],"reveal":true}}`,
	} {
		t.Run(name, func(t *testing.T) {
			if _, err := Parse(caps(t, grant)); err == nil {
				t.Fatal("Parse accepted a malformed grant")
			}
		})
	}
}

func TestParseOneBadGrantFailsAll(t *testing.T) {
	_, err := Parse(caps(t, file(t, "bob.json"), `{"tls":[{"hosts":["a.com"],"ports":[443],"extra":1}]}`))
	if err == nil {
		t.Fatal("a malformed second grant must invalidate the policy")
	}
}

func TestParseNoCaps(t *testing.T) {
	for name, cm := range map[string]tailcfg.PeerCapMap{
		"nil":       nil,
		"other cap": {"example.com/cap/other": {`{}`}},
		"empty":     {CapName: {}},
	} {
		if _, err := Parse(cm); !errors.Is(err, ErrNoCaps) {
			t.Errorf("%s: err = %v, want ErrNoCaps", name, err)
		}
	}
}

func TestParseBrokerCeiling(t *testing.T) {
	if p := mustParse(t, file(t, "alice.json")); p.Broker != nil {
		t.Fatalf("no broker key, ceiling = %+v", p.Broker)
	}
	if p := mustParse(t, `{"broker":{}}`); p.Broker == nil || len(p.Broker.Groups) != 0 || len(p.Broker.Modes) != 0 {
		t.Fatalf("empty broker key, ceiling = %+v", p.Broker)
	}
	p := mustParse(t,
		`{"broker":{"groups":["agents","ops"],"modes":["inject"]}}`,
		`{"tls":[{"hosts":["a.com"],"ports":[443]}]}`,
		`{"broker":{"groups":["readers","agents"],"modes":["inject"]}}`,
	)
	want := &BrokerCeiling{Groups: []string{"agents", "ops", "readers"}, Modes: []BrokerMode{BrokerInject}}
	if !reflect.DeepEqual(p.Broker, want) {
		t.Fatalf("ceiling = %+v, want %+v", p.Broker, want)
	}
}

func TestParsePrivateUnion(t *testing.T) {
	open := `{"private":{"block":false}}`
	if p := mustParse(t, open, open); p.Private.Block {
		t.Error("every grant opening private ranges should open them")
	}
	for _, order := range [][]string{{open, `{}`}, {`{}`, open}} {
		if p := mustParse(t, order...); !p.Private.Block {
			t.Errorf("%v: a grant that does not open private ranges keeps them blocked", order)
		}
	}
	p := mustParse(t, `{"private":{"allow":["10.1.0.0/16"]}}`, `{"private":{"allow":["192.168.5.0/24"]}}`)
	if len(p.Private.Allow) != 2 || !p.Private.Block {
		t.Errorf("private = %+v", p.Private)
	}
}

var (
	alice  = netip.MustParseAddrPort("100.100.1.1:40000")
	egress = netip.MustParseAddr("100.100.9.9")
	lanIP  = netip.MustParseAddr("10.9.8.50")
	floor  = NewFloor([]netip.Addr{egress, lanIP})
)

func ap(s string) netip.AddrPort { return netip.MustParseAddrPort(s) }

func TestDecide(t *testing.T) {
	alicePolicy := mustParse(t, file(t, "alice.json"))
	githubRule := &alicePolicy.HTTP[0]
	merged := mustParse(t,
		`{"tls":[{"hosts":["*.example.com"],"ports":[443]}],"deny":{"hosts":["bad.example.com"]}}`,
		`{"tls":[{"hosts":["bad.example.com","other.org"],"ports":[443,8443]}],"tcp":[{"cidrs":["203.0.113.0/24"],"ports":[5432]}],"deny":{"cidrs":["198.51.100.0/24"]}}`,
		`{"tcp":[{"cidrs":["198.51.100.7/32","10.1.2.3/32","192.168.1.2/32"],"ports":[443,22]}],"private":{"allow":["10.1.0.0/16"]},"dns":{"allow":"rules"}}`,
	)
	httpUnion := mustParse(t,
		`{"http":[{"hosts":["api.test"],"ports":[443],"methods":["GET"]},{"hosts":["api.test"],"ports":[8443]}]}`,
		`{"http":[{"hosts":["*.test"],"ports":[443],"paths":["/v1/*"]},{"hosts":["other.test"],"ports":[443]}]}`,
	)
	wildHTTP := mustParse(t, `{"http":[{"hosts":["*"],"ports":[80]}],"deny":{"hosts":["blocked.example.com"]}}`)
	denyAllTCP := mustParse(t, `{"tcp":[{"cidrs":["0.0.0.0/0"],"ports":[80,443]}],"udp":[{"cidrs":["0.0.0.0/0"],"ports":[443]}],"deny":{"hosts":["*"]},"dns":{"allow":"rules"}}`)
	opened := mustParse(t, `{"private":{"block":false},"tcp":[{"cidrs":["10.0.0.0/8"],"ports":[22]}]}`)
	globDeny := mustParse(t, `{"tcp":[{"cidrs":["0.0.0.0/0"],"ports":[80,443]}],"deny":{"hosts":["api-*.example.com"]}}`)
	public := "140.82.112.3"

	tests := []struct {
		name string
		p    *Policy
		fl   Flow
		want Verdict
	}{
		// Allow rules from the live caps.
		{"tls sni passthrough", alicePolicy, Flow{Proto: TCP, Dst: ap(public + ":443"), SNI: "registry.npmjs.org"}, PassThrough{Rule: "g0.tls0", Name: "registry.npmjs.org"}},
		{"sni normalized", alicePolicy, Flow{Proto: TCP, Dst: ap(public + ":443"), SNI: "GitHub.com."}, PassThrough{Rule: "g0.tls0", Name: "github.com"}},
		{"sni wildcard subdomain", alicePolicy, Flow{Proto: TCP, Dst: ap(public + ":443"), SNI: "raw.githubusercontent.com"}, PassThrough{Rule: "g0.tls0", Name: "raw.githubusercontent.com"}},
		{"sni wildcard apex", alicePolicy, Flow{Proto: TCP, Dst: ap(public + ":443"), SNI: "githubusercontent.com"}, PassThrough{Rule: "g0.tls0", Name: "githubusercontent.com"}},
		{"sni wildcard is not a suffix match", alicePolicy, Flow{Proto: TCP, Dst: ap(public + ":443"), SNI: "evilgithubusercontent.com"}, Deny{ReasonNoRule}},
		{"sni not in any rule", alicePolicy, Flow{Proto: TCP, Dst: ap(public + ":443"), SNI: "example.org"}, Deny{ReasonNoRule}},
		{"tls without sni", alicePolicy, Flow{Proto: TCP, Dst: ap(public + ":443")}, Deny{ReasonNoRule}},
		{"tls rule port mismatch", alicePolicy, Flow{Proto: TCP, Dst: ap(public + ":8443"), SNI: "github.com"}, Deny{ReasonNoRule}},
		{"http rule host decrypts", alicePolicy, Flow{Proto: TCP, Dst: ap(public + ":443"), SNI: "api.github.com"}, Decrypt{Rules: []*HTTPRule{githubRule}, Name: "api.github.com", TLS: true}},
		{"plain http host without rule", alicePolicy, Flow{Proto: TCP, Dst: ap(public + ":80"), HTTPHost: "github.com"}, Deny{ReasonNoRule}},
		{"plain http host on http rule port", mustParse(t, `{"http":[{"hosts":["example.com"],"ports":[80]}]}`), Flow{Proto: TCP, Dst: ap(public + ":80"), HTTPHost: "example.com"}, Decrypt{Rules: []*HTTPRule{&mustParse(t, `{"http":[{"hosts":["example.com"],"ports":[80]}]}`).HTTP[0]}, Name: "example.com"}},
		{"plain http to an https rule port is not tls", alicePolicy, Flow{Proto: TCP, Dst: ap(public + ":443"), HTTPHost: "api.github.com"}, Decrypt{Rules: []*HTTPRule{githubRule}, Name: "api.github.com"}},
		{"plain http host is not sni", mustParse(t, `{"tls":[{"hosts":["example.com"],"ports":[80]}]}`), Flow{Proto: TCP, Dst: ap(public + ":80"), HTTPHost: "example.com"}, Deny{ReasonNoRule}},
		{"tcp by learned name", alicePolicy, Flow{Proto: TCP, Dst: ap(public + ":22"), DstNames: []string{"github.com"}}, Relay{Rule: "g0.tcp0"}},
		{"tcp without learned name", alicePolicy, Flow{Proto: TCP, Dst: ap(public + ":22")}, Deny{ReasonNoRule}},
		{"tcp port mismatch", alicePolicy, Flow{Proto: TCP, Dst: ap(public + ":2222"), DstNames: []string{"github.com"}}, Deny{ReasonNoRule}},
		{"udp by cidr", alicePolicy, Flow{Proto: UDP, Dst: ap("162.159.200.1:123")}, Relay{Rule: "g0.udp0"}},
		{"udp outside cidr", alicePolicy, Flow{Proto: UDP, Dst: ap("162.159.200.2:123")}, Deny{ReasonNoRule}},
		{"udp rule is not tcp", alicePolicy, Flow{Proto: TCP, Dst: ap("162.159.200.1:123")}, Deny{ReasonNoRule}},
		{"udp 443 to http host dropped", mustParse(t, file(t, "alice.json"), `{"udp":[{"cidrs":["0.0.0.0/0"],"ports":[443]}]}`), Flow{Proto: UDP, Dst: ap(public + ":443"), DstNames: []string{"api.github.com"}}, Deny{ReasonQUIC}},
		{"udp 443 to tls host follows udp rules", mustParse(t, file(t, "alice.json"), `{"udp":[{"cidrs":["0.0.0.0/0"],"ports":[443]}]}`), Flow{Proto: UDP, Dst: ap(public + ":443"), DstNames: []string{"github.com"}}, Relay{Rule: "g1.udp0"}},

		// Union across grants.
		{"union first grant", merged, Flow{Proto: TCP, Dst: ap(public + ":443"), SNI: "www.example.com"}, PassThrough{Rule: "g0.tls0", Name: "www.example.com"}},
		{"union second grant port", merged, Flow{Proto: TCP, Dst: ap(public + ":8443"), SNI: "other.org"}, PassThrough{Rule: "g1.tls0", Name: "other.org"}},
		{"union tcp from second grant", merged, Flow{Proto: TCP, Dst: ap("203.0.113.9:5432")}, Relay{Rule: "g1.tcp0"}},
		{"decrypt carries every http rule for host and port", httpUnion, Flow{Proto: TCP, Dst: ap(public + ":443"), SNI: "api.test"}, Decrypt{Rules: []*HTTPRule{&httpUnion.HTTP[0], &httpUnion.HTTP[2]}, Name: "api.test", TLS: true}},
		{"union dns from third grant", merged, Flow{Proto: UDP, Dst: ap("1.1.1.1:53"), DNSQuestion: "www.example.com"}, Relay{Rule: "dns"}},

		// Deny first, across grants.
		{"deny host beats another grant's allow", merged, Flow{Proto: TCP, Dst: ap(public + ":443"), SNI: "bad.example.com"}, Deny{ReasonDenyHost}},
		{"deny cidr beats another grant's allow", merged, Flow{Proto: TCP, Dst: ap("198.51.100.7:22")}, Deny{ReasonDenyCIDR}},
		{"deny host on learned name", merged, Flow{Proto: TCP, Dst: ap("203.0.113.9:5432"), DstNames: []string{"bad.example.com"}}, Deny{ReasonDenyHost}},
		{"extra trailing dot is malformed", wildHTTP, Flow{Proto: TCP, Dst: ap(public + ":80"), HTTPHost: "blocked.example.com.."}, Deny{ReasonMalformed}},
		{"empty label is malformed", wildHTTP, Flow{Proto: TCP, Dst: ap(public + ":80"), HTTPHost: "a..example.com"}, Deny{ReasonMalformed}},
		{"leading dot is malformed", wildHTTP, Flow{Proto: TCP, Dst: ap(public + ":80"), HTTPHost: ".example.com"}, Deny{ReasonMalformed}},
		{"malformed host is not relayed past a wildcard deny", denyAllTCP, Flow{Proto: TCP, Dst: ap(public + ":80"), HTTPHost: "a..example.com"}, Deny{ReasonMalformed}},
		{"malformed sni is not relayed past a wildcard deny", denyAllTCP, Flow{Proto: TCP, Dst: ap(public + ":443"), SNI: "example.com.."}, Deny{ReasonMalformed}},
		{"root host is malformed", denyAllTCP, Flow{Proto: TCP, Dst: ap(public + ":80"), HTTPHost: "."}, Deny{ReasonMalformed}},
		{"zoned host is malformed", denyAllTCP, Flow{Proto: TCP, Dst: ap(public + ":80"), HTTPHost: "fe80::1%example.com"}, Deny{ReasonMalformed}},
		{"malformed learned name", denyAllTCP, Flow{Proto: UDP, Dst: ap(public + ":443"), DstNames: []string{".example.com"}}, Deny{ReasonMalformed}},
		{"malformed dns question", denyAllTCP, Flow{Proto: UDP, Dst: ap("1.1.1.1:53"), DNSQuestion: "a..example.com"}, Deny{ReasonMalformed}},
		{"wildcard deny still relays a nameless flow by cidr", denyAllTCP, Flow{Proto: TCP, Dst: ap(public + ":80")}, Relay{Rule: "g0.tcp0"}},
		{"slash in host escapes no glob deny", globDeny, Flow{Proto: TCP, Dst: ap(public + ":80"), HTTPHost: "api-a/b.example.com"}, Deny{ReasonMalformed}},
		{"slash in sni escapes no glob deny", globDeny, Flow{Proto: TCP, Dst: ap(public + ":443"), SNI: "api-a/b.example.com"}, Deny{ReasonMalformed}},
		{"space in sni is malformed", globDeny, Flow{Proto: TCP, Dst: ap(public + ":443"), SNI: "api-a b.example.com"}, Deny{ReasonMalformed}},
		{"control byte in sni is malformed", globDeny, Flow{Proto: TCP, Dst: ap(public + ":443"), SNI: "api-a\x00.example.com"}, Deny{ReasonMalformed}},
		{"colon in sni is malformed", denyAllTCP, Flow{Proto: TCP, Dst: ap(public + ":443"), SNI: "::1"}, Deny{ReasonMalformed}},
		{"stray colons in host escape no host deny", wildHTTP, Flow{Proto: TCP, Dst: ap(public + ":80"), HTTPHost: "blocked.example.com:80:80"}, Deny{ReasonMalformed}},
		{"empty port in host is malformed", wildHTTP, Flow{Proto: TCP, Dst: ap(public + ":80"), HTTPHost: "blocked.example.com:"}, Deny{ReasonMalformed}},
		{"non-numeric port in host is malformed", wildHTTP, Flow{Proto: TCP, Dst: ap(public + ":80"), HTTPHost: "blocked.example.com:http"}, Deny{ReasonMalformed}},
		{"bare ipv6 host is malformed", denyAllTCP, Flow{Proto: TCP, Dst: ap(public + ":80"), HTTPHost: "::1"}, Deny{ReasonMalformed}},
		{"zoned bracketed ipv6 host is malformed", denyAllTCP, Flow{Proto: TCP, Dst: ap(public + ":80"), HTTPHost: "[fe80::1%25eth0]:80"}, Deny{ReasonMalformed}},
		{"bracketed ipv6 host meets a wildcard deny", denyAllTCP, Flow{Proto: TCP, Dst: ap(public + ":80"), HTTPHost: "[::1]:80"}, Deny{ReasonDenyHost}},
		{"host port is not part of the name", wildHTTP, Flow{Proto: TCP, Dst: ap(public + ":80"), HTTPHost: "Blocked.Example.com:80"}, Deny{ReasonDenyHost}},
		{"underscore label is a name", denyAllTCP, Flow{Proto: TCP, Dst: ap(public + ":443"), SNI: "_acme.example.com"}, Deny{ReasonDenyHost}},
		{"single trailing dot is the fqdn", wildHTTP, Flow{Proto: TCP, Dst: ap(public + ":80"), HTTPHost: "blocked.example.com."}, Deny{ReasonDenyHost}},
		{"deny host on dns", merged, Flow{Proto: UDP, Dst: ap("1.1.1.1:53"), DNSQuestion: "bad.example.com"}, Deny{ReasonDenyHost}},

		// Built-in floor.
		{"floor loopback", mustParse(t, `{"tcp":[{"cidrs":["0.0.0.0/0"],"ports":[80]}]}`), Flow{Proto: TCP, Dst: ap("127.0.0.1:80")}, Deny{ReasonFloor}},
		{"floor loopback v6", mustParse(t, `{"tcp":[{"cidrs":["::/0"],"ports":[80]}]}`), Flow{Proto: TCP, Dst: ap("[::1]:80")}, Deny{ReasonFloor}},
		{"floor metadata", mustParse(t, `{"tcp":[{"cidrs":["169.254.169.254/32"],"ports":[80]}]}`), Flow{Proto: TCP, Dst: ap("169.254.169.254:80")}, Deny{ReasonFloor}},
		{"floor v4-mapped metadata", mustParse(t, `{"tcp":[{"cidrs":["::/0"],"ports":[80]}]}`), Flow{Proto: TCP, Dst: ap("[::ffff:169.254.169.254]:80")}, Deny{ReasonFloor}},
		{"floor egress tailnet address", mustParse(t, `{"tcp":[{"cidrs":["0.0.0.0/0"],"ports":[22]}]}`), Flow{Proto: TCP, Dst: netip.AddrPortFrom(egress, 22)}, Deny{ReasonFloor}},
		{"floor egress lan address beats private allow", mustParse(t, `{"private":{"block":false},"tcp":[{"cidrs":["10.0.0.0/8"],"ports":[22]}]}`), Flow{Proto: TCP, Dst: netip.AddrPortFrom(lanIP, 22)}, Deny{ReasonFloor}},
		{"floor tailnet range", mustParse(t, `{"tcp":[{"cidrs":["100.64.0.0/10"],"ports":[22]}]}`), Flow{Proto: TCP, Dst: ap("100.100.2.2:22")}, Deny{ReasonFloor}},
		{"floor applies to tls passthrough", mustParse(t, `{"tls":[{"hosts":["*"],"ports":[443]}]}`), Flow{Proto: TCP, Dst: ap("127.0.0.1:443"), SNI: "example.com"}, Deny{ReasonFloor}},

		// Private ranges.
		{"private blocked by default", merged, Flow{Proto: TCP, Dst: ap("192.168.1.2:22")}, Deny{ReasonPrivate}},
		{"private allow reopens", merged, Flow{Proto: TCP, Dst: ap("10.1.2.3:22")}, Relay{Rule: "g2.tcp0"}},
		{"private open in every grant", opened, Flow{Proto: TCP, Dst: ap("10.20.30.40:22")}, Relay{Rule: "g0.tcp0"}},
		{"private v6 ula blocked", mustParse(t, `{"tcp":[{"cidrs":["::/0"],"ports":[22]}]}`), Flow{Proto: TCP, Dst: ap("[fd12::1]:22")}, Deny{ReasonPrivate}},

		// DNS.
		{"dns covered by http rule", alicePolicy, Flow{Proto: UDP, Dst: ap("10.0.0.1:53"), DNSQuestion: "api.github.com."}, Relay{Rule: "dns"}},
		{"dns covered by tcp host rule", alicePolicy, Flow{Proto: TCP, Dst: ap("8.8.8.8:53"), DNSQuestion: "github.com"}, Relay{Rule: "dns"}},
		{"dns uncovered", alicePolicy, Flow{Proto: UDP, Dst: ap("10.0.0.1:53"), DNSQuestion: "example.org"}, Deny{ReasonDNSUncovered}},
		{"dns disabled", mustParse(t, `{"tls":[{"hosts":["a.com"],"ports":[443]}]}`), Flow{Proto: UDP, Dst: ap("10.0.0.1:53"), DNSQuestion: "a.com"}, Deny{ReasonDNSDisabled}},

		// No policy.
		{"zero policy denies tls", &Policy{}, Flow{Proto: TCP, Dst: ap(public + ":443"), SNI: "github.com"}, Deny{ReasonNoRule}},
		{"zero policy denies dns", &Policy{}, Flow{Proto: UDP, Dst: ap("10.0.0.1:53"), DNSQuestion: "github.com"}, Deny{ReasonDNSDisabled}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tt.fl.Src = alice
			if got := floor.Decide(tt.p, tt.fl); !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("Decide = %#v, want %#v", got, tt.want)
			}
		})
	}
}

func TestDialControl(t *testing.T) {
	p := mustParse(t, `{"deny":{"cidrs":["198.51.100.0/24"]},"private":{"allow":["10.1.0.0/16"]}}`)
	check := floor.DialControl(p)
	for addr, want := range map[string]Reason{
		"140.82.112.3:443":     "",
		"10.1.2.3:443":         "",
		"127.0.0.1:443":        ReasonFloor,
		"[::ffff:127.0.0.1]:1": ReasonFloor,
		"[fe80::1%eth0]:443":   ReasonFloor,
		"198.51.100.1:443":     ReasonDenyCIDR,
		"10.2.0.1:443":         ReasonPrivate,
		"not-an-ip:443":        ReasonFloor,
	} {
		err := check("tcp", addr, syscall.RawConn(nil))
		var de *DenyError
		switch {
		case want == "" && err != nil:
			t.Errorf("%s: unexpected %v", addr, err)
		case want != "" && (!errors.As(err, &de) || de.Reason != want):
			t.Errorf("%s: err = %v, want reason %s", addr, err, want)
		}
	}
}

// An inject header net/http or ReverseProxy strips on purpose, or one that
// routes the request, is refused: the egress would put it back.
func TestParseRejectsReservedInjectHeaders(t *testing.T) {
	for _, hdr := range []string{
		"Connection", "keep-alive", "Proxy-Authorization", "Proxy-Custom", "TE", "Te", "Trailer",
		"Transfer-Encoding", "Upgrade", "Host", "Forwarded", "X-Forwarded-For", "x-forwarded-host", "Via", "Content-Length",
		"Cookie", "expect",
	} {
		t.Run(hdr, func(t *testing.T) {
			grant := `{"http":[{"hosts":["a.com"],"ports":[443],"credentials":[{"name":"T","secret":"literal:x","inject":{"header":"` + hdr + `","format":"{secret}"}}]}]}`
			if _, err := Parse(caps(t, grant)); err == nil {
				t.Fatal("Parse accepted a reserved inject header")
			}
		})
	}
	mustParse(t, `{"http":[{"hosts":["a.com"],"ports":[443],"credentials":[{"name":"T","secret":"literal:x","inject":{"header":"X-Proxy-Token","format":"{secret}"}}]}]}`)
}

func TestReservedHeadersArePinned(t *testing.T) {
	want := []string{
		"Connection", "Content-Length", "Cookie", "Expect", "Forwarded", "Host", "Keep-Alive",
		"Proxy-", "Te", "Trailer", "Transfer-Encoding", "Upgrade", "Via", "X-Forwarded-",
	}
	if !slices.Equal(reservedHeaders, want) {
		t.Fatalf("reservedHeaders = %q; change ts-broker.domain/reserved-headers with it", reservedHeaders)
	}
}

// An inject format can carry a static key, so a refusal never quotes it.
func TestParseErrorsDoNotQuoteTheFormat(t *testing.T) {
	const key = "sk-static-key-value"
	for name, format := range map[string]string{
		"no placeholder": "Bearer " + key,
		"two":            key + " {secret} {secret}",
		"control byte":   key + " {secret}\r\nX: y",
		"whitespace":     " " + key + " {secret}",
	} {
		t.Run(name, func(t *testing.T) {
			grant := `{"http":[{"hosts":["a.com"],"ports":[443],"credentials":[{"name":"T","secret":"literal:x","inject":{"header":"A","format":` + strconv.Quote(format) + `}}]}]}`
			_, err := Parse(caps(t, grant))
			if err == nil || strings.Contains(err.Error(), key) {
				t.Fatalf("Parse error = %v", err)
			}
		})
	}
}
