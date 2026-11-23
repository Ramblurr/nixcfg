package egress

import (
	"bytes"
	"context"
	"net/netip"
	"sync"
	"testing"
	"time"

	"gvisor.dev/gvisor/pkg/tcpip/header"
	"gvisor.dev/gvisor/pkg/tcpip/stack"
	"tailscale.com/net/packet"
	"tailscale.com/tsnet"
)

// TestExitNodeSendsNothingFromAnUnusedDestination probes routed
// destinations that no flow uses with segments that open no flow. A
// kernel answers those with a RST only from an address it owns; the
// egress owns none of them, so it must send nothing back.
func TestExitNodeSendsNothingFromAnUnusedDestination(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()

	control := testControl(t)
	s, _ := newServer(nil, "")
	egress := tsnetNode(t, control, "egress", "tag:egress")
	if err := s.ExitNode(ctx, egress, time.Minute); err != nil {
		t.Fatal(err)
	}
	cap := captureTCP(t, egress)
	delivered := netstackOf(t, egress).Stats().IP.PacketsDelivered
	before := delivered.Value()

	tun := egress.Sys().Tun.Get()
	src := netip.MustParseAddr("100.100.1.1")
	probes := []header.TCPFlags{header.TCPFlagAck, header.TCPFlagAck | header.TCPFlagFin, header.TCPFlagAck | header.TCPFlagPsh}
	for i, flags := range probes {
		dst := netip.AddrFrom4([4]byte{198, 51, 100, byte(200 + i)})
		if _, err := tun.Write([][]byte{tcp4(src, dst, 10000, 7000, flags, 1)}, 0); err != nil {
			t.Fatal(err)
		}
	}
	for deadline := time.Now().Add(5 * time.Second); delivered.Value()-before < uint64(len(probes)); time.Sleep(20 * time.Millisecond) {
		if time.Now().After(deadline) {
			t.Fatalf("netstack took %d of %d probes", delivered.Value()-before, len(probes))
		}
	}
	time.Sleep(500 * time.Millisecond) // replies leave netstack asynchronously
	if b := cap.since(0, func([]byte) bool { return true }); b != nil {
		id := tcpTuple(b)
		t.Fatalf("the egress sent %v from %v:%d to %v:%d", tcpFlags(b), id.RemoteAddress, id.RemotePort, id.LocalAddress, id.LocalPort)
	}
}

// tcpCapture records the IPv4 TCP packets an egress receives from peers and
// sends toward them, in order.
type tcpCapture struct {
	mu   sync.Mutex
	in   [][]byte
	sent [][]byte
}

func captureTCP(t *testing.T, s *tsnet.Server) *tcpCapture {
	t.Helper()
	c := &tcpCapture{}
	tun := s.Sys().Tun.Get()
	tun.InstallCaptureHook(func(path packet.CapturePath, _ time.Time, b []byte, _ packet.CaptureMeta) {
		if len(b) < header.IPv4MinimumSize || header.IPVersion(b) != header.IPv4Version || header.IPv4(b).TransportProtocol() != header.TCPProtocolNumber {
			return
		}
		b = bytes.Clone(b)
		c.mu.Lock()
		defer c.mu.Unlock()
		switch path {
		case packet.FromPeer:
			c.in = append(c.in, b)
		case packet.SynthesizedToPeer, packet.FromLocal:
			c.sent = append(c.sent, b)
		}
	})
	t.Cleanup(func() { tun.InstallCaptureHook(nil) })
	return c
}

func (c *tcpCapture) first(match func([]byte) bool) []byte {
	c.mu.Lock()
	defer c.mu.Unlock()
	for _, b := range c.in {
		if match(b) {
			return b
		}
	}
	return nil
}

func (c *tcpCapture) last(match func([]byte) bool) []byte {
	c.mu.Lock()
	defer c.mu.Unlock()
	for i := len(c.in) - 1; i >= 0; i-- {
		if match(c.in[i]) {
			return c.in[i]
		}
	}
	return nil
}

// received returns how many packets the egress has received that match.
func (c *tcpCapture) received(match func([]byte) bool) int {
	c.mu.Lock()
	defer c.mu.Unlock()
	n := 0
	for _, b := range c.in {
		if match(b) {
			n++
		}
	}
	return n
}

// mark returns how many packets the egress has sent so far, for since.
func (c *tcpCapture) mark() int {
	c.mu.Lock()
	defer c.mu.Unlock()
	return len(c.sent)
}

// since returns the first packet the egress sent after mark that matches.
func (c *tcpCapture) since(mark int, match func([]byte) bool) []byte {
	c.mu.Lock()
	defer c.mu.Unlock()
	for _, b := range c.sent[mark:] {
		if match(b) {
			return b
		}
	}
	return nil
}

// tcpTuple returns an IPv4 TCP packet's tuple as its receiver registers it.
func tcpTuple(b []byte) stack.TransportEndpointID {
	ip := header.IPv4(b)
	h := header.TCP(b[ip.HeaderLength():])
	return stack.TransportEndpointID{
		LocalAddress: ip.DestinationAddress(), LocalPort: h.DestinationPort(),
		RemoteAddress: ip.SourceAddress(), RemotePort: h.SourcePort(),
	}
}

func tcpFlags(b []byte) header.TCPFlags {
	return header.TCP(b[header.IPv4(b).HeaderLength():]).Flags()
}
