package main

import (
	"context"
	"errors"
	"net/http"
	"net/netip"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"tailscale.com/client/local"
	"tailscale.com/ipn"
	"tailscale.com/tailcfg"
	"tailscale.com/tailcfg/nodecap"
	"tailscale.com/tsnet"
	"tailscale.com/types/logger"
)

// A restarted node comes up Running from its netmap disk cache before
// control answers. The exit node joined after that cache was written, so
// the cache holds no peers. The exit node named by hostname must still be
// selected once control sends the live netmap.
func TestSetExitNodeByNameOnCachedStart(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()

	// Every new control connection waits while hold is locked.
	var hold sync.RWMutex
	control := startControl(t, func(h http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			hold.RLock()
			hold.RUnlock()
			h.ServeHTTP(w, r)
		})
	})
	controlURL := control.HTTPTestServer.URL

	dir := filepath.Join(t.TempDir(), "plug")
	if err := os.MkdirAll(dir, 0o700); err != nil {
		t.Fatal(err)
	}
	newPlug := func() *tsnet.Server {
		s := &tsnet.Server{Dir: dir, ControlURL: controlURL, Hostname: "plug", Logf: logger.Discard}
		t.Cleanup(func() { s.Close() })
		return s
	}

	first := newPlug()
	firstSt, err := first.Up(ctx)
	if err != nil {
		t.Fatal(err)
	}
	firstLC, err := first.LocalClient()
	if err != nil {
		t.Fatal(err)
	}
	control.SetNodeCapMap(firstSt.Self.PublicKey, tailcfg.NodeCapMap{nodecap.CacheNetworkMaps: nil})
	waitForNetmapCache(t, ctx, firstLC)
	first.Close()

	egress, _ := startNode(t, ctx, controlURL, "egress")
	egressLC, err := egress.LocalClient()
	if err != nil {
		t.Fatal(err)
	}
	egressSt, err := egressLC.Status(ctx)
	if err != nil {
		t.Fatal(err)
	}
	control.SetSubnetRoutes(egressSt.Self.PublicKey, []netip.Prefix{
		netip.MustParsePrefix("0.0.0.0/0"),
		netip.MustParsePrefix("::/0"),
	})

	hold.Lock()
	var releaseOnce sync.Once
	release := func() { releaseOnce.Do(hold.Unlock) }
	t.Cleanup(release)

	plug := newPlug()
	if _, err := plug.Up(ctx); err != nil {
		t.Fatal(err)
	}
	lc, err := plug.LocalClient()
	if err != nil {
		t.Fatal(err)
	}
	if st, err := lc.Status(ctx); err != nil {
		t.Fatal(err)
	} else if len(st.Peer) != 0 {
		t.Fatalf("plug came up with %d peers before control answered, want none", len(st.Peer))
	}

	// Control answers a second after ts-plug starts looking for the peer.
	time.AfterFunc(time.Second, release)
	if err := setExitNode(ctx, lc, "egress"); err != nil {
		t.Fatal(err)
	}
	prefs, err := lc.GetPrefs(ctx)
	if err != nil {
		t.Fatal(err)
	}
	if prefs.ExitNodeID != egressSt.Self.ID {
		t.Errorf("ExitNodeID = %q, want egress %q", prefs.ExitNodeID, egressSt.Self.ID)
	}
}

// waitForNetmapCache waits until the node holds the capability that makes
// tailscaled cache its netmap on disk. The netmap that carries it is stored
// before Status can report it.
func waitForNetmapCache(t *testing.T, ctx context.Context, lc *local.Client) {
	t.Helper()
	for {
		st, err := lc.Status(ctx)
		if err == nil && st.Self.HasCap(nodecap.CacheNetworkMaps) {
			return
		}
		select {
		case <-ctx.Done():
			t.Fatalf("node never got %s: %v", nodecap.CacheNetworkMaps, ctx.Err())
		case <-time.After(20 * time.Millisecond):
		}
	}
}

// A name no peer has fails with an error naming it once the wait ends.
func TestSetExitNodeUnknownNameFailsAtDeadline(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()
	plug, _ := startNode(t, ctx, startControl(t, nil).HTTPTestServer.URL, "plug")
	lc, err := plug.LocalClient()
	if err != nil {
		t.Fatal(err)
	}

	wait, stop := context.WithTimeout(ctx, 500*time.Millisecond)
	defer stop()
	start := time.Now()
	err = setExitNode(wait, lc, "nosuch")
	if err == nil || !strings.Contains(err.Error(), `"nosuch"`) {
		t.Fatalf("setExitNode(nosuch) = %v, want an error naming it", err)
	}
	if took := time.Since(start); took < 500*time.Millisecond || took > 5*time.Second {
		t.Fatalf("setExitNode returned after %v, want just after its 500ms deadline", took)
	}
}

// The node's own IP is refused at once instead of waiting for a netmap that
// cannot fix it.
func TestSetExitNodeOwnIPFailsAtOnce(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()
	plug, _ := startNode(t, ctx, startControl(t, nil).HTTPTestServer.URL, "plug")
	lc, err := plug.LocalClient()
	if err != nil {
		t.Fatal(err)
	}
	self, _ := plug.TailscaleIPs()

	start := time.Now()
	err = setExitNode(ctx, lc, self.String())
	if !errors.As(err, new(ipn.ExitNodeLocalIPError)) {
		t.Fatalf("setExitNode(own IP) = %v, want ExitNodeLocalIPError", err)
	}
	if took := time.Since(start); took > 5*time.Second {
		t.Fatalf("setExitNode(own IP) took %v", took)
	}
}
