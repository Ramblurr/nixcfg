package egress

import (
	"cmp"
	"context"
	"errors"
	"log/slog"
	"net"
	"net/netip"
	"syscall"
	"time"

	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/policy"
)

const (
	udpQueue = 64

	defaultMaxUDPSessions          = 8192
	defaultMaxUDPSessionsPerCaller = 1024

	// dnsLinger is how long a DNS session waits for another query, such
	// as the AAAA glibc sends beside an A on the same port. Netstack's
	// MagicDNS uses the same bound.
	dnsLinger = 150 * time.Millisecond

	// udpDropWarnEvery bounds how often flows dropped at the session
	// bounds are logged.
	udpDropWarnEvery = 10 * time.Second
)

// handleUDP carries one UDP session: the datagrams one client address
// sends to one original destination. Netstack gives each session its own
// connected conn, whose writes come from dst. A denied session swallows
// its datagrams until idle so the verdict is logged once per flow.
func (s *Server) handleUDP(ctx context.Context, src, dst netip.AddrPort, c net.Conn) {
	defer c.Close()
	stop := context.AfterFunc(ctx, func() { _ = c.Close() })
	defer stop()
	ok, dropped := s.claimUDP(src.Addr())
	if !ok {
		if dropped > 0 {
			slog.Warn("udp session limit reached, dropping flows", "src", src, "dst", dst, "dropped", dropped)
		}
		return
	}
	defer s.releaseUDP(src.Addr())

	caller := s.IDs.Lookup(ctx, src.Addr())
	if caller.Reason == "" && dst.Port() == policy.DNSPort {
		s.serveDNSUDP(ctx, src, dst, c)
		return
	}
	rec := &record{start: time.Now(), caller: caller, proto: policy.UDP, src: src, dst: dst}
	defer s.log(rec)
	if caller.Reason != "" {
		rec.verdict = policy.Deny{Reason: caller.Reason}
		s.drain(c)
		return
	}
	fl := policy.Flow{
		Proto:    policy.UDP,
		Src:      src,
		Dst:      dst,
		DstNames: s.Names.For(src.Addr(), dst.Addr()),
	}
	rec.verdict = s.Floor.Decide(&caller.Policy, fl)
	if _, ok := rec.verdict.(policy.Relay); !ok {
		s.drain(c)
		return
	}
	up, err := s.dialer(caller)(ctx, "udp", dst.String())
	if err != nil {
		rec.fail(err)
		s.drain(c)
		return
	}
	defer up.Close()
	s.relayUDP(ctx, rec, fl, c, up)
}

// claimUDP takes a session slot for caller, or reports that the bounds
// allow no other. Once per udpDropWarnEvery a refusal also returns the
// number of flows dropped since the last such report, to be logged.
func (s *Server) claimUDP(caller netip.Addr) (ok bool, dropped int) {
	s.udpMu.Lock()
	defer s.udpMu.Unlock()
	if s.perCaller == nil {
		s.perCaller = map[netip.Addr]int{}
	}
	if s.udpSessions >= cmp.Or(s.MaxUDPSessions, defaultMaxUDPSessions) ||
		s.perCaller[caller] >= cmp.Or(s.MaxUDPSessionsPerCaller, defaultMaxUDPSessionsPerCaller) {
		s.udpDropped++
		if now := time.Now(); now.Sub(s.udpWarned) >= udpDropWarnEvery {
			dropped, s.udpDropped, s.udpWarned = s.udpDropped, 0, now
		}
		return false, dropped
	}
	s.udpSessions++
	s.perCaller[caller]++
	return true, 0
}

func (s *Server) releaseUDP(caller netip.Addr) {
	s.udpMu.Lock()
	defer s.udpMu.Unlock()
	s.udpSessions--
	if s.perCaller[caller]--; s.perCaller[caller] <= 0 {
		delete(s.perCaller, caller)
	}
}

// relayUDP copies datagrams both ways until the session idles out, ctx
// ends, or a periodic recheck under the caller's current grants denies it.
func (s *Server) relayUDP(ctx context.Context, rec *record, fl policy.Flow, c, up net.Conn) {
	idle := time.NewTimer(s.UDPIdle)
	defer idle.Stop()
	recheck := time.NewTicker(s.recheckEvery())
	defer recheck.Stop()
	done := make(chan struct{})
	defer close(done)
	fromClient := readLoop(done, c)
	fromUp := readLoop(done, up)
	for {
		select {
		case pkt, ok := <-fromClient:
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
			if n, err := c.Write(pkt); err == nil {
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

// serveDNSUDP answers each query in the session, and ends it once no other
// query follows within dnsLinger. Most clients send each query from a new
// port, so holding the session for UDPIdle would pin a slot per lookup. A
// later query on the same port opens a new session. Each query is its own
// audit line.
func (s *Server) serveDNSUDP(ctx context.Context, src, dst netip.AddrPort, c net.Conn) {
	buf := make([]byte, 65535)
	for {
		_ = c.SetReadDeadline(time.Now().Add(dnsLinger))
		n, err := c.Read(buf)
		if err != nil {
			return
		}
		rec := &record{start: time.Now(), proto: policy.UDP, src: src, dst: dst, up: int64(n)}
		resp, err := s.answerDNS(ctx, rec, "udp", buf[:n])
		rec.err = err
		if resp != nil {
			if n, err := c.Write(resp); err == nil {
				rec.down = int64(n)
			}
		}
		s.log(rec)
	}
}

// drain swallows a denied session's datagrams until it idles out or c is
// closed.
func (s *Server) drain(c net.Conn) {
	buf := make([]byte, 65535)
	for {
		_ = c.SetReadDeadline(time.Now().Add(s.UDPIdle))
		if _, err := c.Read(buf); err != nil {
			return
		}
	}
}

// readLoop reads c into a channel until c fails or done ends. A connected
// UDP socket reports an ICMP unreachable as ECONNREFUSED on a later read,
// which does not end the session.
func readLoop(done <-chan struct{}, c net.Conn) <-chan []byte {
	ch := make(chan []byte, udpQueue)
	go func() {
		defer close(ch)
		buf := make([]byte, 65535)
		for {
			n, err := c.Read(buf)
			if errors.Is(err, syscall.ECONNREFUSED) {
				continue
			}
			if err != nil {
				return
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
