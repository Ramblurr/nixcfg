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
)

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
	_, err = lc.EditPrefs(ctx, &ipn.MaskedPrefs{
		Prefs:              ipn.Prefs{AdvertiseRoutes: tsaddr.ExitRoutes()},
		AdvertiseRoutesSet: true,
	})
	return err
}
