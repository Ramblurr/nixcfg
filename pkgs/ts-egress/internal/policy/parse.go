package policy

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/netip"
	"net/textproto"
	"path"
	"regexp"
	"slices"
	"strings"
	"unicode"

	"golang.org/x/net/http/httpguts"
	"tailscale.com/tailcfg"
)

// ErrNoCaps means the caller has no egress grant at all.
var ErrNoCaps = errors.New("no " + CapName + " capability")

// Parse merges every egress grant in caps into one Policy. Any unknown field
// or malformed value fails the whole policy; the caller then gets the zero
// Policy, which allows nothing.
func Parse(caps tailcfg.PeerCapMap) (Policy, error) {
	grants, err := tailcfg.UnmarshalCapJSON[wireGrant](caps, CapName)
	if err != nil {
		return Policy{}, err
	}
	if len(grants) == 0 {
		return Policy{}, ErrNoCaps
	}
	var p Policy
	for i, g := range grants {
		if err := p.merge(i, g); err != nil {
			return Policy{}, fmt.Errorf("grant %d: %w", i, err)
		}
	}
	return p, nil
}

type wireGrant struct {
	Deny    *wireDeny    `json:"deny"`
	Private *wirePrivate `json:"private"`
	HTTP    []wireHTTP   `json:"http"`
	TLS     []wireHost   `json:"tls"`
	TCP     []wireL4     `json:"tcp"`
	UDP     []wireL4     `json:"udp"`
	DNS     *wireDNS     `json:"dns"`
	Broker  *wireBroker  `json:"broker"`
}

// UnmarshalJSON rejects unknown fields and duplicate keys at every depth,
// and trailing data.
func (g *wireGrant) UnmarshalJSON(b []byte) error {
	if err := rejectDuplicateKeys(json.NewDecoder(bytes.NewReader(b))); err != nil {
		return err
	}
	type plain wireGrant
	dec := json.NewDecoder(bytes.NewReader(b))
	dec.DisallowUnknownFields()
	if err := dec.Decode((*plain)(g)); err != nil {
		return err
	}
	if _, err := dec.Token(); err != io.EOF {
		return errors.New("trailing data after grant")
	}
	return nil
}

// rejectDuplicateKeys walks one JSON value and fails on an object that
// repeats a key. encoding/json keeps the last of them and matches keys
// case-insensitively, so a later "deny": null or "Deny": null would clear
// an earlier deny.
func rejectDuplicateKeys(dec *json.Decoder) error {
	tok, err := dec.Token()
	if err != nil {
		return err
	}
	switch tok {
	case json.Delim('{'):
		seen := map[string]bool{}
		for dec.More() {
			k, err := dec.Token()
			if err != nil {
				return err
			}
			folded := strings.Map(func(r rune) rune { return unicode.ToLower(unicode.ToUpper(r)) }, k.(string))
			if seen[folded] {
				return fmt.Errorf("duplicate key %q", k)
			}
			seen[folded] = true
			if err := rejectDuplicateKeys(dec); err != nil {
				return err
			}
		}
	case json.Delim('['):
		for dec.More() {
			if err := rejectDuplicateKeys(dec); err != nil {
				return err
			}
		}
	default:
		return nil
	}
	_, err = dec.Token()
	return err
}

type wireDeny struct {
	CIDRs []string `json:"cidrs"`
	Hosts []string `json:"hosts"`
}

type wirePrivate struct {
	Block *bool    `json:"block"`
	Allow []string `json:"allow"`
}

type wireHTTP struct {
	Hosts       []string         `json:"hosts"`
	Ports       []uint16         `json:"ports"`
	Methods     []string         `json:"methods"`
	Paths       []string         `json:"paths"`
	Credentials []wireCredential `json:"credentials"`
}

type wireCredential struct {
	Name   string      `json:"name"`
	Secret string      `json:"secret"`
	Inject *wireInject `json:"inject"`
}

type wireInject struct {
	Header string `json:"header"`
	Format string `json:"format"`
}

type wireHost struct {
	Hosts []string `json:"hosts"`
	Ports []uint16 `json:"ports"`
}

type wireL4 struct {
	Hosts []string `json:"hosts"`
	CIDRs []string `json:"cidrs"`
	Ports []uint16 `json:"ports"`
}

type wireDNS struct {
	Allow string `json:"allow"`
}

type wireBroker struct {
	Groups []string `json:"groups"`
	Modes  []string `json:"modes"`
}

func (p *Policy) merge(gi int, g wireGrant) error {
	block := true
	if g.Private != nil {
		if g.Private.Block != nil {
			block = *g.Private.Block
		}
		allow, err := parsePrefixes(g.Private.Allow)
		if err != nil {
			return fmt.Errorf("private: %w", err)
		}
		p.Private.Allow = append(p.Private.Allow, allow...)
	}
	p.Private.Block = p.Private.Block || block

	if g.Deny != nil {
		cidrs, err := parsePrefixes(g.Deny.CIDRs)
		if err != nil {
			return fmt.Errorf("deny: %w", err)
		}
		hosts, err := parseHosts(g.Deny.Hosts, false)
		if err != nil {
			return fmt.Errorf("deny: %w", err)
		}
		p.Deny.CIDRs = append(p.Deny.CIDRs, cidrs...)
		p.Deny.Hosts = append(p.Deny.Hosts, hosts...)
	}

	for i, w := range g.HTTP {
		r, err := parseHTTP(w)
		if err != nil {
			return fmt.Errorf("http[%d]: %w", i, err)
		}
		r.ID = fmt.Sprintf("g%d.http%d", gi, i)
		p.HTTP = append(p.HTTP, r)
	}
	for i, w := range g.TLS {
		hosts, err := parseHosts(w.Hosts, true)
		if err != nil {
			return fmt.Errorf("tls[%d]: %w", i, err)
		}
		ports, err := parsePorts(w.Ports)
		if err != nil {
			return fmt.Errorf("tls[%d]: %w", i, err)
		}
		p.TLS = append(p.TLS, HostRule{ID: fmt.Sprintf("g%d.tls%d", gi, i), Hosts: hosts, Ports: ports})
	}
	for _, l4 := range []struct {
		name string
		in   []wireL4
		out  *[]L4Rule
	}{{"tcp", g.TCP, &p.TCP}, {"udp", g.UDP, &p.UDP}} {
		for i, w := range l4.in {
			r, err := parseL4(w)
			if err != nil {
				return fmt.Errorf("%s[%d]: %w", l4.name, i, err)
			}
			r.ID = fmt.Sprintf("g%d.%s%d", gi, l4.name, i)
			*l4.out = append(*l4.out, r)
		}
	}

	if g.DNS != nil {
		if DNSAllow(g.DNS.Allow) != DNSRules {
			return fmt.Errorf("dns: allow must be %q, got %q", DNSRules, g.DNS.Allow)
		}
		p.DNS.Allow = DNSRules
	}

	if g.Broker != nil {
		if err := p.mergeBroker(*g.Broker); err != nil {
			return fmt.Errorf("broker: %w", err)
		}
	}
	return nil
}

var groupRE = regexp.MustCompile(`^[A-Za-z0-9][A-Za-z0-9_.-]*$`)

func (p *Policy) mergeBroker(w wireBroker) error {
	if p.Broker == nil {
		p.Broker = &BrokerCeiling{}
	}
	b := p.Broker
	for _, g := range w.Groups {
		if !groupRE.MatchString(g) {
			return fmt.Errorf("invalid group %q", g)
		}
		b.Groups = append(b.Groups, g)
	}
	for _, m := range w.Modes {
		if BrokerMode(m) != BrokerInject {
			return fmt.Errorf("mode must be %q, got %q", BrokerInject, m)
		}
		b.Modes = append(b.Modes, BrokerMode(m))
	}
	slices.Sort(b.Groups)
	slices.Sort(b.Modes)
	b.Groups, b.Modes = slices.Compact(b.Groups), slices.Compact(b.Modes)
	return nil
}

var (
	methodRE = regexp.MustCompile(`^[A-Z]+$`)
	credRE   = regexp.MustCompile(`^[A-Z_][A-Z0-9_]*$`)
)

func parseHTTP(w wireHTTP) (HTTPRule, error) {
	hosts, err := parseHosts(w.Hosts, true)
	if err != nil {
		return HTTPRule{}, err
	}
	ports, err := parsePorts(w.Ports)
	if err != nil {
		return HTTPRule{}, err
	}
	for _, m := range w.Methods {
		if !methodRE.MatchString(m) {
			return HTTPRule{}, fmt.Errorf("invalid method %q", m)
		}
	}
	for _, pth := range w.Paths {
		if !strings.HasPrefix(pth, "/") {
			return HTTPRule{}, fmt.Errorf("path %q must start with /", pth)
		}
		if _, err := path.Match(pth, ""); err != nil {
			return HTTPRule{}, fmt.Errorf("invalid path %q: %w", pth, err)
		}
	}
	creds := make([]Credential, 0, len(w.Credentials))
	for i, c := range w.Credentials {
		cred, err := parseCredential(c)
		if err != nil {
			return HTTPRule{}, fmt.Errorf("credentials[%d]: %w", i, err)
		}
		creds = append(creds, cred)
	}
	return HTTPRule{Hosts: hosts, Ports: ports, Methods: w.Methods, Paths: w.Paths, Credentials: creds}, nil
}

func parseCredential(c wireCredential) (Credential, error) {
	secret, err := parseSecretRef(c.Secret)
	if err != nil {
		return Credential{}, err
	}
	if c.Inject == nil {
		return Credential{}, errors.New("missing inject")
	}
	return NewCredential(c.Name, secret, c.Inject.Header, c.Inject.Format)
}

// NewCredential checks a credential's name and how it is injected, the
// same for one from the policy and one the broker names. format holds
// "{secret}" once.
func NewCredential(name string, secret SecretRef, header, format string) (Credential, error) {
	if !credRE.MatchString(name) {
		return Credential{}, fmt.Errorf("invalid name %q", name)
	}
	if !httpguts.ValidHeaderFieldName(header) || ReservedHeader(textproto.CanonicalMIMEHeaderKey(header)) {
		return Credential{}, fmt.Errorf("invalid inject header %q", header)
	}
	// The format can carry a static key, so errors never quote it.
	if strings.Count(format, "{secret}") != 1 {
		return Credential{}, errors.New("inject format must contain {secret} once")
	}
	if !SentVerbatim(format) {
		return Credential{}, errors.New("inject format is not a header value net/http sends as is")
	}
	return Credential{Name: name, Secret: secret, Inject: Inject{Header: textproto.CanonicalMIMEHeaderKey(header), Format: format}}, nil
}

// reservedHeaders are the canonical names, and with a trailing "-" the
// name prefixes, of the headers that frame the request, are hop-by-hop,
// route or forward it, or carry state the tenant should not get a secret
// into.
var reservedHeaders = []string{
	"Connection", "Content-Length", "Cookie", "Expect", "Forwarded", "Host", "Keep-Alive",
	"Proxy-", "Te", "Trailer", "Transfer-Encoding", "Upgrade", "Via", "X-Forwarded-",
}

// ReservedHeader reports whether the canonical header name k is in
// reservedHeaders. net/http and ReverseProxy set or strip most of these on
// purpose, so no credential may be injected there and no placeholder sent
// in one.
func ReservedHeader(k string) bool {
	return slices.ContainsFunc(reservedHeaders, func(r string) bool {
		if strings.HasSuffix(r, "-") {
			return strings.HasPrefix(k, r)
		}
		return k == r
	})
}

// SentVerbatim reports whether net/http sends v byte for byte as a header
// value. It refuses control bytes and trims surrounding whitespace, and a
// secret sent as other bytes than the scrubber looks for is echoed back
// unscrubbed.
func SentVerbatim(v string) bool {
	return httpguts.ValidHeaderFieldValue(v) && textproto.TrimString(v) == v
}

func parseSecretRef(s string) (SecretRef, error) {
	if v, ok := strings.CutPrefix(s, "literal:"); ok && v != "" {
		if !SentVerbatim(v) {
			return SecretRef{}, errors.New("literal secret is not a header value net/http sends as is")
		}
		return SecretRef{Scheme: SecretLiteral, Ref: v}, nil
	}
	if op, err := ParseOPRef(s); err == nil {
		return SecretRef{Scheme: SecretOnePassword, Ref: s, OP: op}, nil
	}
	return SecretRef{}, fmt.Errorf("secret must be op://vault/item/[section/]field or literal:value")
}

// ParseOPRef parses op://vault/item/[section/]field. Each part may be a
// 1Password ID or a title. Adapted from iron-proxy's ParseOPRef
// (internal/transform/secrets/op_connect_resolver.go, Apache-2.0, see
// NOTICE): the same rules, without its error wording.
func ParseOPRef(ref string) (OPRef, error) {
	rest, ok := strings.CutPrefix(ref, "op://")
	if !ok {
		return OPRef{}, fmt.Errorf("%q does not start with op://", ref)
	}
	parts := strings.Split(rest, "/")
	if len(parts) < 3 || len(parts) > 4 || slices.Contains(parts, "") {
		return OPRef{}, fmt.Errorf("%q is not op://vault/item/[section/]field", ref)
	}
	out := OPRef{Vault: parts[0], Item: parts[1], Field: parts[len(parts)-1]}
	if len(parts) == 4 {
		out.Section = parts[2]
	}
	return out, nil
}

func parseL4(w wireL4) (L4Rule, error) {
	hosts, err := parseHosts(w.Hosts, false)
	if err != nil {
		return L4Rule{}, err
	}
	cidrs, err := parsePrefixes(w.CIDRs)
	if err != nil {
		return L4Rule{}, err
	}
	if len(hosts) == 0 && len(cidrs) == 0 {
		return L4Rule{}, errors.New("needs hosts or cidrs")
	}
	ports, err := parsePorts(w.Ports)
	if err != nil {
		return L4Rule{}, err
	}
	return L4Rule{Hosts: hosts, CIDRs: cidrs, Ports: ports}, nil
}

func parseHosts(in []string, required bool) ([]HostPattern, error) {
	if required && len(in) == 0 {
		return nil, errors.New("hosts must not be empty")
	}
	out := make([]HostPattern, 0, len(in))
	for _, s := range in {
		h, err := ParseHostPattern(s)
		if err != nil {
			return nil, err
		}
		out = append(out, h)
	}
	return out, nil
}

func parsePorts(in []uint16) ([]uint16, error) {
	if len(in) == 0 {
		return nil, errors.New("ports must not be empty")
	}
	for _, p := range in {
		if p == 0 {
			return nil, errors.New("port 0 is invalid")
		}
	}
	return in, nil
}

// parsePrefixes requires CIDR notation so a bare IP cannot be mistaken for
// a range. IPv4-mapped IPv6 ranges are refused: flow addresses are unmapped
// before matching, so such a range would never match.
func parsePrefixes(in []string) ([]netip.Prefix, error) {
	out := make([]netip.Prefix, 0, len(in))
	for _, s := range in {
		if !strings.Contains(s, "/") {
			return nil, fmt.Errorf("invalid CIDR %q: needs a prefix length", s)
		}
		p, err := netip.ParsePrefix(s)
		if err != nil {
			return nil, fmt.Errorf("invalid CIDR %q: %w", s, err)
		}
		if p.Addr().Is4In6() {
			return nil, fmt.Errorf("invalid CIDR %q: use the IPv4 form", s)
		}
		out = append(out, p.Masked())
	}
	return out, nil
}
