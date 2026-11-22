package policy

import "net/netip"

type Proto string

const (
	TCP Proto = "tcp"
	UDP Proto = "udp"
)

// Flow is what the runtime knows about one connection or datagram flow
// before it is decided. Names are normalized by Decide.
type Flow struct {
	Proto Proto
	Src   netip.AddrPort
	Dst   netip.AddrPort
	// SNI from a peeked TLS ClientHello.
	SNI string
	// HTTPHost is the Host value of a peeked plain HTTP request, as sent.
	HTTPHost string
	// DNSQuestion is the queried name on a port 53 flow.
	DNSQuestion string
	// DstNames are names this egress resolved to Dst for the same caller.
	DstNames []string
}

// Verdict is one of Deny, PassThrough, Relay or Decrypt.
type Verdict interface{ verdict() }

// Deny drops the flow.
type Deny struct{ Reason Reason }

// PassThrough splices the flow undecrypted to Name on the flow's port,
// dialled by name so the client cannot pick the address.
type PassThrough struct {
	Rule string
	Name string
}

// Relay forwards the flow to its original destination. On port 53 it means
// forward the query to the upstream resolver.
type Relay struct{ Rule string }

// Decrypt hands the flow to HTTP-level handling: TLS termination when TLS
// is set, plain HTTP otherwise. Rules holds every HTTP rule for Name on the
// flow's port, so each request can be matched against all of them. The
// runtime denies it when it has no Decrypter.
type Decrypt struct {
	Rules []*HTTPRule
	Name  string
	TLS   bool
}

func (Deny) verdict()        {}
func (PassThrough) verdict() {}
func (Relay) verdict()       {}
func (Decrypt) verdict()     {}

// Reason says why a flow was denied.
type Reason string

const (
	ReasonFloor        Reason = "floor"
	ReasonDenyCIDR     Reason = "deny-cidr"
	ReasonDenyHost     Reason = "deny-host"
	ReasonMalformed    Reason = "malformed-name"
	ReasonPrivate      Reason = "private"
	ReasonNoRule       Reason = "no-rule"
	ReasonQUIC         Reason = "udp443-to-http-host"
	ReasonDNSDisabled  Reason = "dns-disabled"
	ReasonDNSUncovered Reason = "dns-not-covered"
	// Runtime reasons, set outside Decide.
	ReasonNoIdentity  Reason = "no-identity"
	ReasonNoCaps      Reason = "no-caps"
	ReasonBadPolicy   Reason = "invalid-policy"
	ReasonUndecrypted Reason = "decrypt-unavailable"
	ReasonIncomplete  Reason = "incomplete-preface"
	ReasonDial        Reason = "dial-failed"
)

// DNSPort flows are answered by the egress itself, whatever their address.
const DNSPort = 53

var (
	// builtinFloor is denied for every caller, whatever the rules say:
	// loopback, link-local (cloud metadata included), the tailnet ranges
	// (tailnet traffic stays under grants, never through egress),
	// unspecified, multicast and broadcast.
	builtinFloor = mustPrefixes(
		"127.0.0.0/8", "::1/128",
		"169.254.0.0/16", "fe80::/10", "fd00:ec2::254/128", "fd20:ce::254/128",
		"100.64.0.0/10", "fd7a:115c:a1e0::/48",
		"0.0.0.0/8", "::/128",
		"224.0.0.0/4", "ff00::/8", "255.255.255.255/32",
	)
	privateRanges = mustPrefixes("10.0.0.0/8", "172.16.0.0/12", "192.168.0.0/16", "fc00::/7")
)

// Floor is the built-in deny set plus the egress host's own addresses.
type Floor struct{ prefixes []netip.Prefix }

func NewFloor(self []netip.Addr) Floor {
	ps := append([]netip.Prefix(nil), builtinFloor...)
	for _, a := range self {
		a = a.Unmap()
		ps = append(ps, netip.PrefixFrom(a, a.BitLen()))
	}
	return Floor{prefixes: ps}
}

// Decide returns the verdict for fl under p. Deny comes first: malformed
// names, floor, deny CIDRs, private ranges, deny hosts. Then allow rules, then deny by default.
func (f Floor) Decide(p *Policy, fl Flow) Verdict {
	// A name no pattern can spell would miss deny.hosts and fall through
	// to rules that ignore names, so the flow is refused before any rule.
	sni, okSNI := flowName(fl.SNI, ParseName)
	host, okHost := flowName(fl.HTTPHost, ParseHost)
	question, okQ := flowName(fl.DNSQuestion, ParseName)
	names := make([]string, 0, len(fl.DstNames))
	ok := okSNI && okHost && okQ
	for _, n := range fl.DstNames {
		n, okN := flowName(n, ParseName)
		names, ok = append(names, n), ok && okN
	}
	if !ok {
		return Deny{ReasonMalformed}
	}
	if fl.Dst.Port() == DNSPort {
		return decideDNS(p, question)
	}
	if r, denied := f.CheckAddr(p, fl.Dst.Addr()); denied {
		return Deny{r}
	}
	for _, n := range append([]string{sni, host}, names...) {
		if n != "" && matchAny(p.Deny.Hosts, n) {
			return Deny{ReasonDenyHost}
		}
	}
	port := fl.Dst.Port()
	switch fl.Proto {
	case TCP:
		if name := firstNonEmpty(sni, host); name != "" {
			if rs := p.httpRules(name, port); len(rs) > 0 {
				return Decrypt{Rules: rs, Name: name, TLS: name == sni}
			}
		}
		if sni != "" {
			if r := p.tlsRule(sni, port); r != nil {
				return PassThrough{Rule: r.ID, Name: sni}
			}
		}
		if r := l4Rule(p.TCP, fl.Dst, names); r != nil {
			return Relay{Rule: r.ID}
		}
	case UDP:
		if port == 443 {
			for _, n := range names {
				if p.hasHTTPHost(n) {
					return Deny{ReasonQUIC}
				}
			}
		}
		if r := l4Rule(p.UDP, fl.Dst, names); r != nil {
			return Relay{Rule: r.ID}
		}
	}
	return Deny{ReasonNoRule}
}

func decideDNS(p *Policy, q string) Verdict {
	switch {
	case p.DNS.Allow != DNSRules:
		return Deny{ReasonDNSDisabled}
	case q == "":
		return Deny{ReasonDNSUncovered}
	case matchAny(p.Deny.Hosts, q):
		return Deny{ReasonDenyHost}
	case p.Covers(q):
		return Relay{Rule: "dns"}
	}
	return Deny{ReasonDNSUncovered}
}

// CheckAddr applies the floor, deny CIDRs and the private block to ip.
func (f Floor) CheckAddr(p *Policy, ip netip.Addr) (Reason, bool) {
	ip = ip.WithZone("").Unmap()
	switch {
	case !ip.IsValid() || prefixesContain(f.prefixes, ip):
		return ReasonFloor, true
	case prefixesContain(p.Deny.CIDRs, ip):
		return ReasonDenyCIDR, true
	case p.Private.Block && prefixesContain(privateRanges, ip) && !prefixesContain(p.Private.Allow, ip):
		return ReasonPrivate, true
	}
	return "", false
}

// flowName parses a name the flow supplied, if it supplied one.
func flowName(raw string, parse func(string) (string, bool)) (string, bool) {
	if raw == "" {
		return "", true
	}
	return parse(raw)
}

func firstNonEmpty(ss ...string) string {
	for _, s := range ss {
		if s != "" {
			return s
		}
	}
	return ""
}

func mustPrefixes(ss ...string) []netip.Prefix {
	out := make([]netip.Prefix, len(ss))
	for i, s := range ss {
		out[i] = netip.MustParsePrefix(s)
	}
	return out
}
