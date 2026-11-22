// Package policy holds the egress rule model: one caller's merged rules
// parsed from Tailscale grant capabilities, the flow being decided, and the
// verdict. Decide is pure; the runtime feeds it facts and acts on the result.
package policy

import (
	"net/netip"
	"slices"
)

// CapName is the grant application capability that carries egress rules.
const CapName = "moot.land/cap/egress"

// Policy is one caller's rules, merged across every grant that targets the
// egress host. The zero value allows nothing.
type Policy struct {
	Deny    DenyList
	Private Private
	HTTP    []HTTPRule
	TLS     []HostRule
	TCP     []L4Rule
	UDP     []L4Rule
	DNS     DNS
}

// DenyList is checked before any allow rule.
type DenyList struct {
	CIDRs []netip.Prefix
	Hosts []HostPattern
}

// Private controls RFC 1918 and ULA destinations. Block is true unless every
// grant sets "block": false. Allow reopens specific ranges while blocked.
type Private struct {
	Block bool
	Allow []netip.Prefix
}

// HTTPRule names hosts whose flows need HTTP-level handling: TLS
// termination on TLS flows, request parsing on plain HTTP. Methods and Paths
// empty mean any.
type HTTPRule struct {
	ID          string
	Hosts       []HostPattern
	Ports       []uint16
	Methods     []string
	Paths       []string
	Credentials []Credential
}

// HostRule admits TLS by SNI and passes it through undecrypted.
type HostRule struct {
	ID    string
	Hosts []HostPattern
	Ports []uint16
}

// L4Rule admits raw TCP or UDP. A destination matches by CIDR, or by a name
// the caller resolved to that address through this egress.
type L4Rule struct {
	ID    string
	Hosts []HostPattern
	CIDRs []netip.Prefix
	Ports []uint16
}

// Credential is injected into requests on decrypted flows.
type Credential struct {
	Name   string
	Secret SecretRef
	Inject Inject
}

// PlaceholderPrefix starts every placeholder. A placeholder is the prefix
// followed by the credential name, and it ends at the first byte that
// cannot be part of a name ([A-Z0-9_]). Tenants hold only the placeholder;
// it is deterministic because it is not a secret.
const PlaceholderPrefix = "TS_EGRESS_PLACEHOLDER_"

// Placeholder is the string a tenant sends in place of the secret.
func (c Credential) Placeholder() string { return PlaceholderPrefix + c.Name }

// SecretRef points at a secret value. The policy never holds a real secret:
// op:// references resolve through 1Password, literal: is for test dummies.
// Ref is the literal value or the whole op:// reference; OP is the parsed
// reference.
type SecretRef struct {
	Scheme SecretScheme
	Ref    string
	OP     OPRef
}

// OPRef is a parsed op://vault/item/[section/]field reference.
type OPRef struct {
	Vault, Item, Section, Field string
}

type SecretScheme string

const (
	SecretOnePassword SecretScheme = "op"
	SecretLiteral     SecretScheme = "literal"
)

// Inject sets Header to Format with "{secret}" replaced by the value.
type Inject struct {
	Header string
	Format string
}

// DNS controls answers to the caller's DNS queries.
type DNS struct {
	Allow DNSAllow
}

type DNSAllow string

const (
	// DNSNone refuses every query.
	DNSNone DNSAllow = ""
	// DNSRules forwards names that some allow rule covers.
	DNSRules DNSAllow = "rules"
)

// Covers reports whether some allow rule names host. DNS forwarding uses it.
func (p *Policy) Covers(host string) bool {
	for _, r := range p.HTTP {
		if matchAny(r.Hosts, host) {
			return true
		}
	}
	for _, r := range p.TLS {
		if matchAny(r.Hosts, host) {
			return true
		}
	}
	for _, rs := range [][]L4Rule{p.TCP, p.UDP} {
		for _, r := range rs {
			if matchAny(r.Hosts, host) {
				return true
			}
		}
	}
	return false
}

// httpRules returns every HTTP rule for host on port, in grant order.
func (p *Policy) httpRules(host string, port uint16) []*HTTPRule {
	var out []*HTTPRule
	for i := range p.HTTP {
		r := &p.HTTP[i]
		if slices.Contains(r.Ports, port) && matchAny(r.Hosts, host) {
			out = append(out, r)
		}
	}
	return out
}

func (p *Policy) hasHTTPHost(host string) bool {
	for _, r := range p.HTTP {
		if matchAny(r.Hosts, host) {
			return true
		}
	}
	return false
}

func (p *Policy) tlsRule(host string, port uint16) *HostRule {
	for i := range p.TLS {
		r := &p.TLS[i]
		if slices.Contains(r.Ports, port) && matchAny(r.Hosts, host) {
			return r
		}
	}
	return nil
}

func l4Rule(rules []L4Rule, dst netip.AddrPort, names []string) *L4Rule {
	for i := range rules {
		r := &rules[i]
		if !slices.Contains(r.Ports, dst.Port()) {
			continue
		}
		if prefixesContain(r.CIDRs, dst.Addr()) {
			return r
		}
		for _, n := range names {
			if matchAny(r.Hosts, n) {
				return r
			}
		}
	}
	return nil
}

func prefixesContain(ps []netip.Prefix, ip netip.Addr) bool {
	ip = ip.WithZone("").Unmap()
	for _, p := range ps {
		if p.Contains(ip) {
			return true
		}
	}
	return false
}
