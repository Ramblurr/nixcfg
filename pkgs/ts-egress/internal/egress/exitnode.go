package egress

import (
	"context"
	"errors"
	"fmt"
	"net"
	"net/netip"
	"time"

	"tailscale.com/ipn"
	"tailscale.com/ipn/ipnlocal"
	"tailscale.com/net/tsaddr"
	"tailscale.com/tsnet"
	"tailscale.com/types/nettype"
	"tailscale.com/wgengine/netstack"

	"gvisor.dev/gvisor/pkg/tcpip"
	"gvisor.dev/gvisor/pkg/tcpip/transport/tcp"
)

// timeWait is how long the exit node holds a TCP connection it closed first
// in TIME_WAIT, instead of gVisor's 60 s. gVisor reopens TIME_WAIT only for
// a listener, and netstack's forwarder is none, so it drops a SYN that
// reuses the tuple. A Linux tenant, the passive closer, has no TIME_WAIT of
// its own and reuses a port to the same destination as soon as its
// ephemeral range comes round, in under a second at times. Linux
// retransmits a SYN 1 s after sending it, so on the low-latency tenant leg
// measured (sub-ms to tens of ms RTT) a reused port usually costs one
// retransmit, not a dial that outlasts the client's timeout. The cost is a
// 1 s window to ack a retransmitted last FIN: on that leg Linux resends it
// twice in 1 s at its 200 ms minimum RTO, but a FIN delayed past 1 s goes
// unacked and leaves the tenant in LAST-ACK until it gives up.
const timeWait = time.Second

// ExitNode starts ts as an exit node whose every TCP and UDP flow s
// decides, and returns once it advertises the exit routes. ts must not be
// started yet, so that peerapi DoH is refused for every caller before the
// node comes up. The node must be tagged: peerapi serves DoH to an untagged
// node's same-owner peers without asking the hook. tsnet calls the fallback
// handlers under its own lock, so they only hand the flow on and the caller
// is looked up per flow.
func (s *Server) ExitNode(ctx context.Context, ts *tsnet.Server, identityTTL time.Duration) error {
	if ts.Sys() != nil {
		return errors.New("tsnet server already started")
	}
	ipnlocal.HookReplyToDNSQueries = nil
	ts.DropTransitPings = true
	lc, err := ts.LocalClient()
	if err != nil {
		return err
	}
	s.IDs = NewIdentities(lc, identityTTL)
	go s.IDs.Watch(ctx, lc)
	ts.RegisterFallbackTCPHandler(func(_, _ netip.AddrPort) (func(net.Conn), bool) {
		return func(c net.Conn) { s.handleTCP(ctx, c) }, true
	})
	ts.RegisterFallbackUDPHandler(func(src, dst netip.AddrPort) (func(nettype.ConnPacketConn), bool) {
		return func(c nettype.ConnPacketConn) { s.handleUDP(ctx, src, dst, c) }, true
	})
	st, err := ts.Up(ctx)
	if err != nil {
		return err
	}
	if st.Self.Tags == nil || st.Self.Tags.Len() == 0 {
		return fmt.Errorf("node %s is untagged; an exit node needs a tag so peerapi DNS stays refused", st.Self.HostName)
	}
	// Before the exit routes, so it covers every flow: an endpoint reads
	// it when it enters TIME_WAIT.
	tw := tcpip.TCPTimeWaitTimeoutOption(timeWait)
	if err := ts.Sys().Netstack.Get().(*netstack.Impl).SetTransportProtocolOption(tcp.ProtocolNumber, &tw); err != nil {
		return fmt.Errorf("setting the TIME_WAIT timeout: %v", err)
	}
	_, err = lc.EditPrefs(ctx, &ipn.MaskedPrefs{
		Prefs:              ipn.Prefs{AdvertiseRoutes: tsaddr.ExitRoutes()},
		AdvertiseRoutesSet: true,
	})
	return err
}
