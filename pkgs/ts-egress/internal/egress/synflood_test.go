package egress

import (
	"context"
	"net/netip"
	"reflect"
	"strconv"
	"sync"
	"testing"
	"time"
	"unsafe"

	"gvisor.dev/gvisor/pkg/tcpip"
	"gvisor.dev/gvisor/pkg/tcpip/header"
	"tailscale.com/metrics"
	"tailscale.com/tsnet"
	"tailscale.com/wgengine/netstack"
)

// TestExitNodeForgetsAnUncompletedSYNFlood floods the egress with SYNs that
// are never acknowledged, past netstack's per-client and global in-flight
// limits, and checks that netstack holds no state for them once the client
// resets every attempt.
func TestExitNodeForgetsAnUncompletedSYNFlood(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()

	control := testControl(t)
	s, _ := newServer(nil, "")
	egress := tsnetNode(t, control, "egress", "tag:egress")
	if err := s.ExitNode(ctx, egress, time.Minute); err != nil {
		t.Fatal(err)
	}

	ns := egress.Sys().Netstack.Get().(*netstack.Impl)
	limit := expvarInt(t, ns, "gauge_tcp_forward_in_flight_limit")
	perClient := expvarInt(t, ns, "gauge_tcp_forward_in_flight_per_client_limit")

	// The sources are no peer's, so the SYN-ACKs go nowhere and every
	// handshake stays in flight until the client resets it.
	tun := egress.Sys().Tun.Get()
	dst := netip.MustParseAddr("198.51.100.200")
	clients := []struct {
		src      netip.Addr
		attempts int
	}{
		{netip.MustParseAddr("100.100.1.1"), perClient + 16},
		{netip.MustParseAddr("100.100.1.2"), limit - perClient + 16},
	}
	send := func(flags header.TCPFlags, seq uint32) {
		for _, c := range clients {
			for i := range c.attempts {
				if _, err := tun.Write([][]byte{tcp4(c.src, dst, uint16(10000+i), 7000, flags, seq)}, 0); err != nil {
					t.Fatal(err)
				}
			}
		}
	}
	send(header.TCPFlagSyn, 0)
	if n := expvarInt(t, ns, "counter_tcp_forward_max_in_flight_per_client_drop"); n == 0 {
		t.Fatal("the flood never reached the per-client limit")
	}
	if n := expvarInt(t, ns, "counter_tcp_forward_max_in_flight_drop"); n == 0 {
		t.Fatal("the flood never reached the global limit")
	}

	var st floodState
	for deadline := time.Now().Add(30 * time.Second); time.Now().Before(deadline); time.Sleep(200 * time.Millisecond) {
		// An attempt only takes the reset once its handshake has begun,
		// so reset them all until netstack lets go.
		send(header.TCPFlagRst, 1)
		if st = netstackFloodState(t, egress); st == (floodState{}) {
			return
		}
	}
	t.Fatalf("netstack still holds state for the flood: %+v", st)
}

// floodState is what netstack holds for in-flight TCP connection attempts.
type floodState struct {
	PacketsInFlight int // entries in the per-endpoint dedup map
	InFlight        int // per-client in-flight counts, summed
	Clients         int // entries in the per-client count map
	TransitAddrs    int // destination addresses assigned to the NIC
}

func netstackFloodState(t *testing.T, s *tsnet.Server) floodState {
	t.Helper()
	ns := s.Sys().Netstack.Get().(*netstack.Impl)
	v := reflect.ValueOf(ns).Elem()
	mu := (*sync.Mutex)(unsafe.Pointer(v.FieldByName("mu").UnsafeAddr()))
	mu.Lock()
	packets := v.FieldByName("packetsInFlight").Len()
	clients := v.FieldByName("connsInFlightByClient").Len()
	mu.Unlock()
	return floodState{
		PacketsInFlight: packets,
		InFlight:        expvarInt(t, ns, "gauge_tcp_forward_in_flight"),
		Clients:         clients,
		TransitAddrs:    len(transitAddrs(t, s)),
	}
}

func expvarInt(t *testing.T, ns *netstack.Impl, name string) int {
	t.Helper()
	v := ns.ExpVar().(*metrics.Set).Get(name)
	if v == nil {
		t.Fatalf("netstack exports no %s", name)
	}
	n, err := strconv.Atoi(v.String())
	if err != nil {
		t.Fatalf("%s: %v", name, err)
	}
	return n
}

func tcp4(src, dst netip.Addr, sport, dport uint16, flags header.TCPFlags, seq uint32) []byte {
	const size = header.IPv4MinimumSize + header.TCPMinimumSize
	srcAddr, dstAddr := tcpip.AddrFrom4(src.As4()), tcpip.AddrFrom4(dst.As4())
	ip := header.IPv4(make([]byte, size))
	ip.Encode(&header.IPv4Fields{
		Protocol:    uint8(header.TCPProtocolNumber),
		TotalLength: size,
		TTL:         64,
		SrcAddr:     srcAddr,
		DstAddr:     dstAddr,
	})
	ip.SetChecksum(^ip.CalculateChecksum())
	seg := header.TCP(ip[header.IPv4MinimumSize:])
	seg.Encode(&header.TCPFields{
		SrcPort:    sport,
		DstPort:    dport,
		SeqNum:     seq,
		DataOffset: header.TCPMinimumSize,
		Flags:      flags,
		WindowSize: 65535,
	})
	seg.SetChecksum(^seg.CalculateChecksum(header.PseudoHeaderChecksum(header.TCPProtocolNumber, srcAddr, dstAddr, header.TCPMinimumSize)))
	return ip
}
