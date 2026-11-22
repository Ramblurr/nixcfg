package main

import (
	"context"
	"errors"
	"fmt"
	"log/slog"
	"net"
	"net/netip"
	"strings"
	"time"

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

// exitNodeWait bounds how long startup waits for -exit-node to resolve. A
// start from a cached netmap waits on control, which reached through a
// proxy can take several seconds to answer.
const exitNodeWait = time.Minute

// setExitNode makes the node's prefs select exitNode, a Tailscale IP or
// peer name, or select none when exitNode is empty. Prefs persist in the
// state directory, so an empty flag clears an exit node an earlier run set.
// It waits until ctx ends for exitNode to resolve to a peer.
func setExitNode(ctx context.Context, lc *local.Client, exitNode string) error {
	var prefs ipn.Prefs
	if exitNode != "" {
		var err error
		if prefs, err = resolveExitNode(ctx, lc, exitNode); err != nil {
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

// resolveExitNode returns prefs that select exitNode once the netmap has a
// peer it names. A node started from its netmap disk cache is Running
// before control sends the live netmap, and that cache may lack the peer,
// so the lookup runs again on every peer change until ctx ends.
func resolveExitNode(ctx context.Context, lc *local.Client, exitNode string) (ipn.Prefs, error) {
	// The watch starts before the first lookup, so a change that lands
	// between a lookup and the next wait still wakes it.
	w, err := lc.WatchIPNBus(ctx, ipn.NotifyInitialState|ipn.NotifyPeerChanges)
	if err != nil {
		return ipn.Prefs{}, fmt.Errorf("watch tailscale: %w", err)
	}
	defer w.Close()

	var lookupErr error
	stopped := func(err error) error {
		if lookupErr != nil && ctx.Err() != nil {
			return fmt.Errorf("exit node %q did not resolve before the deadline: %w", exitNode, lookupErr)
		}
		return err
	}
	for {
		if _, err := w.Next(); err != nil {
			return ipn.Prefs{}, stopped(fmt.Errorf("watch tailscale: %w", err))
		}
		status, err := lc.Status(ctx)
		if err != nil {
			return ipn.Prefs{}, stopped(fmt.Errorf("get tailscale status: %w", err))
		}
		var prefs ipn.Prefs
		lookupErr = prefs.SetExitNodeIP(exitNode, status)
		if lookupErr == nil {
			return prefs, nil
		}
		// No netmap makes the node's own address a peer.
		if errors.As(lookupErr, new(ipn.ExitNodeLocalIPError)) {
			return ipn.Prefs{}, lookupErr
		}
	}
}
