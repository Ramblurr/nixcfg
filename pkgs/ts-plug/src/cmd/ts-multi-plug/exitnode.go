package main

import (
	"context"
	"errors"
	"fmt"
	"log/slog"
	"net"
	"net/netip"
	"strings"

	"tailscale.com/client/local"
	"tailscale.com/ipn"
	"tailscale.com/tsnet"
	"tailscale.com/wgengine/netstack"
)

// netstackDial dials dst over the tailnet and never over the host network.
// A destination outside the tailnet leaves through the exit node, and fails
// when there is none.
type netstackDial func(ctx context.Context, network string, dst netip.AddrPort) (net.Conn, error)

func newNetstackDial(ts *tsnet.Server) (netstackDial, error) {
	ns, ok := ts.Sys().Netstack.Get().(*netstack.Impl)
	if !ok {
		return nil, errors.New("tsnet server has no netstack")
	}
	return func(ctx context.Context, network string, dst netip.AddrPort) (net.Conn, error) {
		dst = unmap(dst)
		if strings.HasPrefix(network, "udp") {
			conn, err := ns.DialContextUDP(ctx, dst)
			if err != nil {
				return nil, err
			}
			return conn, nil
		}
		conn, err := ns.DialContextTCP(ctx, dst)
		if err != nil {
			return nil, err
		}
		return conn, nil
	}, nil
}

func unmap(ap netip.AddrPort) netip.AddrPort {
	return netip.AddrPortFrom(ap.Addr().Unmap(), ap.Port())
}

// setExitNode makes the node's prefs select exitNode, a Tailscale IP or
// peer name, or select none when exitNode is empty. Prefs persist in the
// state directory, so an empty flag clears an exit node an earlier run set.
func setExitNode(ctx context.Context, lc *local.Client, exitNode string) error {
	var prefs ipn.Prefs
	if exitNode != "" {
		status, err := lc.Status(ctx)
		if err != nil {
			return fmt.Errorf("get tailscale status: %w", err)
		}
		if err := prefs.SetExitNodeIP(exitNode, status); err != nil {
			return err
		}
	}
	got, err := lc.EditPrefs(ctx, &ipn.MaskedPrefs{
		Prefs:         prefs,
		ExitNodeIDSet: true,
		ExitNodeIPSet: true,
	})
	if err != nil {
		return err
	}
	if exitNode == "" {
		return nil
	}
	if got.ExitNodeID.IsZero() {
		return fmt.Errorf("exit node %q does not resolve to a peer", exitNode)
	}
	slog.Info("using exit node", slog.String("exit_node", exitNode), slog.String("id", string(got.ExitNodeID)))
	return nil
}
