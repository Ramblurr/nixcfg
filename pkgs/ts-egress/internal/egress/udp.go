package egress

import (
	"cmp"
	"context"
	"errors"
	"log/slog"
	"net"
	"net/netip"
	"time"

	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/policy"
)

type udpKey struct{ src, dst netip.AddrPort }

// udpSession is one client address talking to one original destination.
// Every datagram for the pair goes through in, in order.
type udpSession struct {
	in chan []byte
}

const (
	udpQueue = 64

	defaultMaxUDPSessions          = 8192
	defaultMaxUDPSessionsPerCaller = 1024
)

// ServeUDP reads TPROXY'd datagrams from pc until it is closed and hands
// each to the session for its (source, original destination) pair.
func (s *Server) ServeUDP(ctx context.Context, pc *net.UDPConn) error {
	buf := make([]byte, 65535)
	oob := make([]byte, 1024)
	for {
		n, oobn, _, from, err := pc.ReadMsgUDPAddrPort(buf, oob)
		if err != nil {
			if errors.Is(err, net.ErrClosed) {
				return nil
			}
			return err
		}
		dst, ok := origDst(oob[:oobn])
		if !ok {
			slog.Debug("udp datagram without original destination", "src", from)
			continue
		}
		src := netip.AddrPortFrom(from.Addr().Unmap(), from.Port())
		s.deliver(ctx, udpKey{src, dst}, buf[:n])
	}
}

// deliver queues a copy of pkt on k's session, starting the session if the
// bounds allow another. A datagram that would exceed them is dropped.
func (s *Server) deliver(ctx context.Context, k udpKey, pkt []byte) {
	s.udpMu.Lock()
	if s.sessions == nil {
		s.sessions = map[udpKey]*udpSession{}
		s.perCaller = map[netip.Addr]int{}
	}
	sess := s.sessions[k]
	if sess == nil {
		caller := k.src.Addr()
		if len(s.sessions) >= cmp.Or(s.MaxUDPSessions, defaultMaxUDPSessions) ||
			s.perCaller[caller] >= cmp.Or(s.MaxUDPSessionsPerCaller, defaultMaxUDPSessionsPerCaller) {
			s.udpMu.Unlock()
			slog.Debug("udp session limit reached, dropping datagram", "src", k.src, "dst", k.dst)
			return
		}
		sess = &udpSession{in: make(chan []byte, udpQueue)}
		s.sessions[k] = sess
		s.perCaller[caller]++
		go s.runUDP(ctx, k, sess)
	}
	s.udpMu.Unlock()
	select {
	case sess.in <- append([]byte(nil), pkt...):
	default:
	}
}

func (s *Server) endUDP(k udpKey) {
	s.udpMu.Lock()
	delete(s.sessions, k)
	if s.perCaller[k.src.Addr()]--; s.perCaller[k.src.Addr()] <= 0 {
		delete(s.perCaller, k.src.Addr())
	}
	s.udpMu.Unlock()
}

// runUDP decides the session, then relays it until it idles out or a
// periodic recheck under the caller's current grants denies it. A denied
// session swallows its datagrams until idle so the verdict is logged once
// per flow.
func (s *Server) runUDP(ctx context.Context, k udpKey, sess *udpSession) {
	defer s.endUDP(k)
	caller := s.IDs.Lookup(ctx, k.src.Addr())
	if caller.Reason == "" && k.dst.Port() == policy.DNSPort {
		s.runDNSUDP(ctx, k, sess)
		return
	}
	rec := &record{start: time.Now(), caller: caller, proto: policy.UDP, src: k.src, dst: k.dst}
	defer s.log(rec)
	if caller.Reason != "" {
		rec.verdict = policy.Deny{Reason: caller.Reason}
		s.drain(ctx, sess)
		return
	}
	fl := policy.Flow{
		Proto:    policy.UDP,
		Src:      k.src,
		Dst:      k.dst,
		DstNames: s.Names.For(k.src.Addr(), k.dst.Addr()),
	}
	rec.verdict = s.Floor.Decide(&caller.Policy, fl)
	if _, ok := rec.verdict.(policy.Relay); !ok {
		s.drain(ctx, sess)
		return
	}
	up, err := s.dial(caller)(ctx, "udp", k.dst.String())
	if err != nil {
		rec.fail(err)
		s.drain(ctx, sess)
		return
	}
	defer up.Close()
	back, err := replyConn(ctx, k.dst, k.src)
	if err != nil {
		rec.err = err
		s.drain(ctx, sess)
		return
	}
	defer back.Close()

	// A connected transparent socket for the pair may also receive the
	// client's later datagrams, so read it as well as the listener queue.
	idle := time.NewTimer(s.UDPIdle)
	defer idle.Stop()
	recheck := time.NewTicker(s.recheckEvery())
	defer recheck.Stop()
	done := make(chan struct{})
	defer close(done)
	fromUp := readLoop(done, up)
	fromBack := readLoop(done, back)
	for {
		select {
		case pkt := <-sess.in:
			if n, err := up.Write(pkt); err == nil {
				rec.up += int64(n)
			}
		case pkt, ok := <-fromBack:
			if !ok {
				return
			}
			if n, err := up.Write(pkt); err == nil {
				rec.up += int64(n)
			}
		case pkt, ok := <-fromUp:
			if !ok {
				return
			}
			if n, err := back.Write(pkt); err == nil {
				rec.down += int64(n)
			}
		case <-recheck.C:
			if v := s.reauthorize(ctx, fl); v != nil {
				rec.verdict = v
				return
			}
			continue
		case <-idle.C:
			return
		case <-ctx.Done():
			return
		}
		idle.Reset(s.UDPIdle)
	}
}

// recheckEvery is how often a live relay is decided again: once per
// identity TTL, so it ends within two TTLs of a revocation.
func (s *Server) recheckEvery() time.Duration {
	if s.IDs.ttl > 0 {
		return s.IDs.ttl
	}
	return time.Second
}

// reauthorize decides fl again under the caller's current grants. It
// returns the deny that ends the flow, or nil while the flow may go on.
func (s *Server) reauthorize(ctx context.Context, fl policy.Flow) policy.Verdict {
	caller := s.IDs.Lookup(ctx, fl.Src.Addr())
	if caller.Reason != "" {
		return policy.Deny{Reason: caller.Reason}
	}
	v := s.Floor.Decide(&caller.Policy, fl)
	if _, ok := v.(policy.Relay); ok {
		return nil
	}
	return v
}

// runDNSUDP answers each query in the session from the original
// destination address. Each query is its own audit line.
func (s *Server) runDNSUDP(ctx context.Context, k udpKey, sess *udpSession) {
	back, err := replyConn(ctx, k.dst, k.src)
	if err != nil {
		logRecord(s.Audit, &record{start: time.Now(), caller: s.IDs.Lookup(ctx, k.src.Addr()), proto: policy.UDP, src: k.src, dst: k.dst, verdict: policy.Deny{Reason: policy.ReasonDial}, err: err})
		s.drain(ctx, sess)
		return
	}
	defer back.Close()
	done := make(chan struct{})
	defer close(done)
	fromBack := readLoop(done, back)
	idle := time.NewTimer(s.UDPIdle)
	defer idle.Stop()
	for {
		var query []byte
		select {
		case query = <-sess.in:
		case pkt, ok := <-fromBack:
			if !ok {
				return
			}
			query = pkt
		case <-idle.C:
			return
		case <-ctx.Done():
			return
		}
		idle.Reset(s.UDPIdle)
		rec := &record{start: time.Now(), proto: policy.UDP, src: k.src, dst: k.dst, up: int64(len(query))}
		resp, err := s.answerDNS(ctx, rec, "udp", query)
		rec.err = err
		if resp != nil {
			if n, err := back.Write(resp); err == nil {
				rec.down = int64(n)
			}
		}
		s.log(rec)
	}
}

// drain swallows a denied session's datagrams until it idles out or ctx
// ends.
func (s *Server) drain(ctx context.Context, sess *udpSession) {
	idle := time.NewTimer(s.UDPIdle)
	defer idle.Stop()
	for {
		select {
		case <-sess.in:
			idle.Reset(s.UDPIdle)
		case <-idle.C:
			return
		case <-ctx.Done():
			return
		}
	}
}

// readLoop reads c into a channel until c is closed or done ends.
func readLoop(done <-chan struct{}, c net.Conn) <-chan []byte {
	ch := make(chan []byte, udpQueue)
	go func() {
		defer close(ch)
		buf := make([]byte, 65535)
		for {
			n, err := c.Read(buf)
			if errors.Is(err, net.ErrClosed) {
				return
			}
			if err != nil {
				continue
			}
			select {
			case ch <- append([]byte(nil), buf[:n]...):
			case <-done:
				return
			}
		}
	}()
	return ch
}
