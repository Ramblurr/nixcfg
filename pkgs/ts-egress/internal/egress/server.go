// Package egress is the runtime around policy.Decide: the tsnet exit node,
// caller identity, DNS filtering, the Tailscale helper proxy and the audit
// log.
//
// Credential injection is outbound: a decrypted request's placeholders are
// swapped for secrets on the way to the upstream. On the way back only the
// response headers of a request that carried a credential are scrubbed:
// the exact bytes of each secret it sent, and the base64 of a Basic
// credential built from a placeholder, become what the tenant sent.
// Response bodies and trailers pass as the upstream sent them, compressed
// or streamed, and so does a secret another request sent. An allowed host
// that echoes a credential in its body hands it to the tenant, so scope
// each credential to hosts without echo endpoints.
package egress

import (
	"context"
	"io"
	"log/slog"
	"net"
	"net/netip"
	"slices"
	"strconv"
	"sync"
	"time"

	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/policy"
)

// Server decides and carries tenant flows.
type Server struct {
	Floor       policy.Floor
	IDs         *Identities
	Names       *Names
	Resolver    string // upstream DNS server, host:port
	PeekTimeout time.Duration
	UDPIdle     time.Duration
	Audit       *slog.Logger
	// Decrypter handles Decrypt verdicts. Nil denies them (108-05 fills it).
	Decrypter Decrypter
	// Dial opens upstream connections for a caller. Nil dials the host
	// network, guarded by the floor and the caller's deny rules.
	Dial func(*Caller) DialFunc
	// MaxUDPSessions and MaxUDPSessionsPerCaller bound live UDP sessions,
	// denied ones included. Zero means the defaults.
	MaxUDPSessions, MaxUDPSessionsPerCaller int

	udpMu       sync.Mutex
	udpSessions int
	perCaller   map[netip.Addr]int
}

// Decrypter terminates TLS (or reads plain HTTP) for a flow that matched an
// HTTP rule. dial returns a dialer guarded by the floor and the given
// caller's deny rules.
type Decrypter interface {
	Serve(ctx context.Context, caller *Caller, v policy.Decrypt, reauth Reauthorize, dst netip.AddrPort, client net.Conn, peeked []byte, dial func(*Caller) DialFunc) (up, down int64, err error)
}

// Reauthorize decides a decrypted flow again under the caller's current
// grants, read through the identity cache.
type Reauthorize func(ctx context.Context) (*Caller, policy.Verdict)

type DialFunc func(ctx context.Context, network, address string) (net.Conn, error)

func (s *Server) dialer(c *Caller) DialFunc {
	if s.Dial != nil {
		return s.Dial(c)
	}
	d := &net.Dialer{
		Timeout: 10 * time.Second,
		Control: s.Floor.DialControl(&c.Policy),
		Resolver: &net.Resolver{
			PreferGo: true,
			Dial: func(ctx context.Context, network, _ string) (net.Conn, error) {
				return (&net.Dialer{}).DialContext(ctx, network, s.Resolver)
			},
		},
	}
	return d.DialContext
}

func (s *Server) handleTCP(ctx context.Context, conn net.Conn) {
	defer conn.Close()
	src, dst := addrPort(conn.RemoteAddr()), addrPort(conn.LocalAddr())
	caller := s.IDs.Lookup(ctx, src.Addr())
	if caller.Reason == "" && dst.Port() == policy.DNSPort {
		s.serveDNSTCP(ctx, src, dst, conn)
		return
	}
	rec := &record{start: time.Now(), caller: caller, proto: policy.TCP, src: src, dst: dst}
	defer s.log(rec)
	if caller.Reason != "" {
		rec.verdict = policy.Deny{Reason: caller.Reason}
		return
	}

	var pk peeked
	if s.wantsPeek(&caller.Policy, rec.dst.Port()) {
		pk = peek(conn, s.PeekTimeout)
	}
	rec.sni, rec.host = pk.SNI, pk.Host
	if pk.Incomplete {
		rec.verdict = policy.Deny{Reason: policy.ReasonIncomplete}
		return
	}
	fl := policy.Flow{
		Proto:    policy.TCP,
		Src:      rec.src,
		Dst:      rec.dst,
		SNI:      pk.SNI,
		HTTPHost: pk.Host,
		DstNames: s.Names.For(rec.src.Addr(), rec.dst.Addr()),
	}
	v := s.Floor.Decide(&caller.Policy, fl)
	rec.verdict = v
	var target string
	switch v := v.(type) {
	case policy.Deny:
		return
	case policy.Decrypt:
		if s.Decrypter == nil {
			rec.verdict = policy.Deny{Reason: policy.ReasonUndecrypted}
			return
		}
		reauth := func(ctx context.Context) (*Caller, policy.Verdict) {
			c := s.IDs.Lookup(ctx, fl.Src.Addr())
			if c.Reason != "" {
				return c, policy.Deny{Reason: c.Reason}
			}
			return c, s.Floor.Decide(&c.Policy, fl)
		}
		rec.up, rec.down, rec.err = s.Decrypter.Serve(ctx, caller, v, reauth, rec.dst, conn, pk.Bytes, s.dialer)
		return
	case policy.PassThrough:
		target = net.JoinHostPort(v.Name, strconv.Itoa(int(rec.dst.Port())))
	case policy.Relay:
		target = rec.dst.String()
	}
	up, err := s.dialer(caller)(ctx, "tcp", target)
	if err != nil {
		rec.fail(err)
		return
	}
	defer up.Close()
	if _, err := up.Write(pk.Bytes); err != nil {
		rec.err = err
		return
	}
	rec.up, rec.down = splice(conn, up)
	rec.up += int64(len(pk.Bytes))
}

// wantsPeek reports whether the destination name could change the verdict
// on this port: a name rule covers it, or a deny host could refuse a flow
// a TCP rule allows by address. Peeking a server-first protocol costs
// PeekTimeout.
func (s *Server) wantsPeek(p *policy.Policy, port uint16) bool {
	if port == 80 || port == 443 {
		return true
	}
	if len(p.Deny.Hosts) > 0 {
		for _, r := range p.TCP {
			if slices.Contains(r.Ports, port) {
				return true
			}
		}
	}
	for _, r := range p.HTTP {
		if slices.Contains(r.Ports, port) {
			return true
		}
	}
	for _, r := range p.TLS {
		if slices.Contains(r.Ports, port) {
			return true
		}
	}
	return false
}

// splice copies both ways until both directions finish, half-closing as
// each side ends. It returns bytes sent client to upstream, and back.
func splice(client, up net.Conn) (sent, recv int64) {
	var wg sync.WaitGroup
	wg.Go(func() { sent = pipe(up, client) })
	recv = pipe(client, up)
	wg.Wait()
	return sent, recv
}

// pipe copies src to dst. At EOF it half-closes dst. On an error it closes
// both, which also ends the copy the other way.
func pipe(dst, src net.Conn) int64 {
	n, err := io.Copy(dst, src)
	if err != nil {
		_ = dst.Close()
		_ = src.Close()
		return n
	}
	closeWrite(dst)
	return n
}

func closeWrite(c net.Conn) {
	if cw, ok := c.(interface{ CloseWrite() error }); ok {
		_ = cw.CloseWrite()
		return
	}
	_ = c.Close()
}

func addrPort(a net.Addr) netip.AddrPort {
	switch a := a.(type) {
	case *net.TCPAddr:
		ap := a.AddrPort()
		return netip.AddrPortFrom(ap.Addr().Unmap(), ap.Port())
	case *net.UDPAddr:
		ap := a.AddrPort()
		return netip.AddrPortFrom(ap.Addr().Unmap(), ap.Port())
	}
	return netip.AddrPort{}
}
