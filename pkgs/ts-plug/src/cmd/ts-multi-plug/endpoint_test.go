package main

import (
	"context"
	"net"
	"net/netip"
	"os"
	"path/filepath"
	"slices"
	"testing"
	"time"

	"tailscale.com/client/local"
	"tailscale.com/ipn/store/mem"
	"tailscale.com/types/logger"
)

func freeUDPPort(t *testing.T) uint16 {
	t.Helper()
	c, err := net.ListenPacket("udp4", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer c.Close()
	return uint16(c.LocalAddr().(*net.UDPAddr).Port)
}

// A peer learns the node's endpoints from control: the static endpoint as
// given, and the node's own socket on the fixed port.
func TestFixedPortAndStaticEndpointReachPeers(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()
	controlURL := startControl(t, nil).HTTPTestServer.URL

	port := freeUDPPort(t)
	static := netip.MustParseAddrPort("192.0.2.19:41700")
	dir := filepath.Join(t.TempDir(), "plug")
	if err := os.MkdirAll(dir, 0o700); err != nil {
		t.Fatal(err)
	}
	plug := newServer("plug", dir, nil, port)
	plug.ControlURL = controlURL
	plug.Store = new(mem.Store)
	plug.Ephemeral = true
	plug.Logf = logger.Discard
	t.Cleanup(func() { plug.Close() })
	if err := startServer(plug, static); err != nil {
		t.Fatal(err)
	}
	st, err := plug.Up(ctx)
	if err != nil {
		t.Fatal(err)
	}

	peer, _ := startNode(t, ctx, controlURL, "peer")
	lc, err := peer.LocalClient()
	if err != nil {
		t.Fatal(err)
	}
	own := netip.AddrPortFrom(netip.MustParseAddr("127.0.0.1"), port).String()
	var addrs []string
	for {
		status, err := lc.Status(ctx)
		if err != nil {
			t.Fatal(err)
		}
		if ps := status.Peer[st.Self.PublicKey]; ps != nil {
			addrs = ps.Addrs
		}
		if slices.Contains(addrs, static.String()) && slices.Contains(addrs, own) {
			return
		}
		select {
		case <-ctx.Done():
			t.Fatalf("peer sees endpoints %q, want %s and %s", addrs, static, own)
		case <-time.After(50 * time.Millisecond):
		}
	}
}

// The LocalAPI socket answers the tailscale CLI's requests for the node.
func TestLocalAPISocketServesTheNode(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()
	controlURL := startControl(t, nil).HTTPTestServer.URL
	plug, _ := startNode(t, ctx, controlURL, "plug")
	lc, err := plug.LocalClient()
	if err != nil {
		t.Fatal(err)
	}

	socket := filepath.Join(t.TempDir(), "localapi.sock")
	failed := make(chan error, 1)
	ln, err := serveLocalAPI(socket, lc.Dial, func(err error) { failed <- err })
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()

	cli := &local.Client{Socket: socket, UseSocketOnly: true}
	got, err := cli.Status(ctx)
	if err != nil {
		t.Fatal(err)
	}
	want, err := lc.Status(ctx)
	if err != nil {
		t.Fatal(err)
	}
	if got.Self.PublicKey != want.Self.PublicKey {
		t.Errorf("socket answered for node %v, want %v", got.Self.PublicKey, want.Self.PublicKey)
	}
	select {
	case err := <-failed:
		t.Fatal(err)
	default:
	}
}
