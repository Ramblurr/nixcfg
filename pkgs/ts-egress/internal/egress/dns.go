package egress

import (
	"context"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"math"
	"net"
	"net/netip"
	"time"

	"golang.org/x/net/dns/dnsmessage"

	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/policy"
)

const dnsTimeout = 5 * time.Second

var errBadQuery = errors.New("malformed DNS query")

// Extended DNS Error codes (RFC 8914) a refusal carries, so a resolver can
// tell a policy decision from a name that does not exist.
const (
	optionEDE     = 15 // EDNS option code of an Extended DNS Error
	edeBlocked    = 15 // the caller's policy does not cover the name
	edeProhibited = 18 // the caller has no usable policy at all
	ednsUDPSize   = 1232
)

// answerDNS decides one query under the grants the caller holds now, so a
// long DNS session cannot outlive a revocation, and returns the response
// to send back. Covered names are forwarded to the upstream resolver over network
// and their A/AAAA answers are learned for raw flows. Everything else gets
// REFUSED, with an Extended DNS Error saying why when the query carried
// EDNS. A query that does not parse gets no answer, and an upstream answer
// that does not validate gets SERVFAIL and teaches nothing.
func (s *Server) answerDNS(ctx context.Context, rec *record, network string, query []byte) ([]byte, error) {
	var p dnsmessage.Parser
	hdr, err := p.Start(query)
	if err != nil || hdr.Response {
		return nil, errBadQuery
	}
	qs, err := p.AllQuestions()
	if err != nil || len(qs) != 1 {
		return nil, errBadQuery
	}
	q := qs[0]
	edns := hasEDNS(&p)
	raw := q.Name.String()
	// Decide sees the name as the query spelled it; the audit line and name
	// learning use the normalized form for every verdict.
	rec.question = policy.NormalizeName(raw)
	caller := s.IDs.Lookup(ctx, rec.src.Addr())
	rec.caller = caller
	if caller.Reason != "" {
		rec.verdict = policy.Deny{Reason: caller.Reason}
		return refused(hdr, q, edns, edeProhibited)
	}
	rec.verdict = s.Floor.Decide(&caller.Policy, policy.Flow{
		Proto:       rec.proto,
		Src:         rec.src,
		Dst:         rec.dst,
		DNSQuestion: raw,
	})
	if _, ok := rec.verdict.(policy.Relay); !ok {
		return refused(hdr, q, edns, edeBlocked)
	}
	resp, err := exchange(ctx, network, s.Resolver, query)
	if err != nil {
		return servfail(hdr, q, edns, err)
	}
	learned, err := parseAnswer(hdr.ID, q, resp)
	if err != nil {
		return servfail(hdr, q, edns, err)
	}
	for _, l := range learned {
		s.Names.Learn(rec.src.Addr(), l.addr, rec.question, l.ttl)
	}
	return resp, nil
}

// maxCNAMEChain bounds how many CNAMEs parseAnswer follows from the
// queried name.
const maxCNAMEChain = 8

type learnedAddr struct {
	addr netip.Addr
	ttl  time.Duration
}

// parseAnswer validates resp as a whole as the answer to q, then returns
// the A and AAAA addresses it gives the queried name: those owned by the
// end of the CNAME chain that starts at the queried name, each with the
// shortest TTL along the chain. Records off that chain are ignored.
func parseAnswer(id uint16, q dnsmessage.Question, resp []byte) ([]learnedAddr, error) {
	var m dnsmessage.Message
	if err := m.Unpack(resp); err != nil {
		return nil, err
	}
	if !m.Response || m.ID != id || len(m.Questions) != 1 || !sameQuestion(m.Questions[0], q) {
		return nil, errors.New("upstream answer does not match the query")
	}
	if m.RCode != dnsmessage.RCodeSuccess {
		return nil, nil
	}
	type cname struct {
		target string
		ttl    uint32
	}
	cnames := map[string]cname{}
	addrs := map[string][]learnedAddr{}
	for _, r := range m.Answers {
		owner := policy.NormalizeName(r.Header.Name.String())
		ttl := time.Duration(r.Header.TTL) * time.Second
		switch b := r.Body.(type) {
		case *dnsmessage.CNAMEResource:
			if _, dup := cnames[owner]; dup {
				return nil, fmt.Errorf("upstream answer has two CNAMEs for %s", owner)
			}
			cnames[owner] = cname{policy.NormalizeName(b.CNAME.String()), r.Header.TTL}
		case *dnsmessage.AResource:
			addrs[owner] = append(addrs[owner], learnedAddr{netip.AddrFrom4(b.A), ttl})
		case *dnsmessage.AAAAResource:
			addrs[owner] = append(addrs[owner], learnedAddr{netip.AddrFrom16(b.AAAA), ttl})
		}
	}
	name, chainTTL := policy.NormalizeName(q.Name.String()), time.Duration(math.MaxInt64)
	for hops := 0; ; hops++ {
		c, ok := cnames[name]
		if !ok {
			break
		}
		if hops == maxCNAMEChain {
			return nil, nil
		}
		name, chainTTL = c.target, min(chainTTL, time.Duration(c.ttl)*time.Second)
	}
	out := addrs[name]
	for i := range out {
		out[i].ttl = min(out[i].ttl, chainTTL)
	}
	return out, nil
}

func sameQuestion(a, b dnsmessage.Question) bool {
	return a.Type == b.Type && a.Class == b.Class &&
		policy.NormalizeName(a.Name.String()) == policy.NormalizeName(b.Name.String())
}

// exchange sends query to server and returns its response. TCP messages
// carry a two-byte length prefix.
func exchange(ctx context.Context, network, server string, query []byte) ([]byte, error) {
	ctx, cancel := context.WithTimeout(ctx, dnsTimeout)
	defer cancel()
	conn, err := (&net.Dialer{}).DialContext(ctx, network, server)
	if err != nil {
		return nil, err
	}
	defer conn.Close()
	deadline, _ := ctx.Deadline()
	_ = conn.SetDeadline(deadline)
	if network == "udp" {
		if _, err := conn.Write(query); err != nil {
			return nil, err
		}
		buf := make([]byte, 65535)
		n, err := conn.Read(buf)
		if err != nil {
			return nil, err
		}
		return buf[:n], nil
	}
	if err := writeTCPMsg(conn, query); err != nil {
		return nil, err
	}
	return readTCPMsg(conn)
}

func readTCPMsg(r io.Reader) ([]byte, error) {
	var l [2]byte
	if _, err := io.ReadFull(r, l[:]); err != nil {
		return nil, err
	}
	msg := make([]byte, binary.BigEndian.Uint16(l[:]))
	_, err := io.ReadFull(r, msg)
	return msg, err
}

func writeTCPMsg(w io.Writer, msg []byte) error {
	buf := binary.BigEndian.AppendUint16(make([]byte, 0, 2+len(msg)), uint16(len(msg)))
	_, err := w.Write(append(buf, msg...))
	return err
}

// serveDNSTCP answers length-prefixed queries on conn until the client
// closes it or sends a query that does not parse. Each query is its own
// audit line.
func (s *Server) serveDNSTCP(ctx context.Context, src, dst netip.AddrPort, c net.Conn) {
	for {
		_ = c.SetReadDeadline(time.Now().Add(30 * time.Second))
		query, err := readTCPMsg(c)
		if err != nil {
			return
		}
		rec := &record{start: time.Now(), proto: policy.TCP, src: src, dst: dst}
		resp, err := s.answerDNS(ctx, rec, "tcp", query)
		rec.up, rec.err = int64(len(query)), err
		if resp != nil {
			rec.down = int64(len(resp))
			err = writeTCPMsg(c, resp)
		}
		s.log(rec)
		if err != nil {
			return
		}
	}
}

// hasEDNS reports whether the query p is parsing, already past its
// questions, carries an OPT record. A reply carries one only if the query
// did (RFC 6891).
func hasEDNS(p *dnsmessage.Parser) bool {
	if p.SkipAllAnswers() != nil || p.SkipAllAuthorities() != nil {
		return false
	}
	for {
		h, err := p.AdditionalHeader()
		if err != nil {
			return false
		}
		if h.Type == dnsmessage.TypeOPT {
			return true
		}
		if p.SkipAdditional() != nil {
			return false
		}
	}
}

func refused(q dnsmessage.Header, question dnsmessage.Question, edns bool, infoCode uint16) ([]byte, error) {
	ede := dnsmessage.Option{Code: optionEDE, Data: []byte{byte(infoCode >> 8), byte(infoCode)}}
	return reply(q, question, dnsmessage.RCodeRefused, edns, ede)
}

func servfail(q dnsmessage.Header, question dnsmessage.Question, edns bool, cause error) ([]byte, error) {
	b, err := reply(q, question, dnsmessage.RCodeServerFailure, edns)
	return b, errors.Join(cause, err)
}

// reply builds a response to q with no answers. With edns it carries an
// OPT record holding opts; without, opts are dropped.
func reply(q dnsmessage.Header, question dnsmessage.Question, rcode dnsmessage.RCode, edns bool, opts ...dnsmessage.Option) ([]byte, error) {
	b := dnsmessage.NewBuilder(nil, dnsmessage.Header{
		ID:                 q.ID,
		Response:           true,
		OpCode:             q.OpCode,
		RecursionDesired:   q.RecursionDesired,
		RecursionAvailable: true,
		RCode:              rcode,
	})
	if err := b.StartQuestions(); err != nil {
		return nil, err
	}
	if err := b.Question(question); err != nil {
		return nil, err
	}
	if edns {
		var rh dnsmessage.ResourceHeader
		if err := rh.SetEDNS0(ednsUDPSize, rcode, false); err != nil {
			return nil, err
		}
		if err := b.StartAdditionals(); err != nil {
			return nil, err
		}
		if err := b.OPTResource(rh, dnsmessage.OPTResource{Options: opts}); err != nil {
			return nil, err
		}
	}
	return b.Finish()
}
