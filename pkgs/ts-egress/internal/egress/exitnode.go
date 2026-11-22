package egress

import (
	"context"
	"net"
	"net/netip"

	"tailscale.com/ipn"
	"tailscale.com/ipn/ipnlocal"
	"tailscale.com/net/tsaddr"
	"tailscale.com/tsnet"
	"tailscale.com/types/nettype"
)

// ExitNode makes ts an exit node whose every TCP and UDP flow s decides, and
// waits until ts is up. Peerapi DoH is refused for every
// caller, so tenant DNS reaches s only on port 53. tsnet calls the fallback
// handlers under its own lock, so they only hand the flow on and the caller
// is looked up per flow.
func (s *Server) ExitNode(ctx context.Context, ts *tsnet.Server) error {
	ipnlocal.HookReplyToDNSQueries = nil
	ts.RegisterFallbackTCPHandler(func(_, _ netip.AddrPort) (func(net.Conn), bool) {
		return func(c net.Conn) { s.handleTCP(ctx, c) }, true
	})
	ts.RegisterFallbackUDPHandler(func(src, dst netip.AddrPort) (func(nettype.ConnPacketConn), bool) {
		return func(c nettype.ConnPacketConn) { s.handleUDP(ctx, src, dst, c) }, true
	})
	if _, err := ts.Up(ctx); err != nil {
		return err
	}
	lc, err := ts.LocalClient()
	if err != nil {
		return err
	}
	_, err = lc.EditPrefs(ctx, &ipn.MaskedPrefs{
		Prefs:              ipn.Prefs{AdvertiseRoutes: tsaddr.ExitRoutes()},
		AdvertiseRoutesSet: true,
	})
	return err
}
