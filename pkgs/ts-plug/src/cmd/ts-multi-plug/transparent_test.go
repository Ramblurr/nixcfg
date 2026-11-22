package main

import (
	"context"
	"net"
	"net/netip"
	"testing"
	"time"
)

// loopbackDial stands in for the tailnet. It dials target whatever the
// original destination, and records each destination it was asked for.
func loopbackDial(target netip.AddrPort, dsts chan<- netip.AddrPort) netstackDial {
	return func(ctx context.Context, network string, dst netip.AddrPort) (net.Conn, error) {
		dsts <- dst
		var d net.Dialer
		return d.DialContext(ctx, network, target.String())
	}
}

func listenUDP(t *testing.T) *net.UDPConn {
	t.Helper()
	conn, err := net.ListenUDP("udp", &net.UDPAddr{IP: net.IPv4(127, 0, 0, 1)})
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { conn.Close() })
	return conn
}

// udpEcho answers each datagram with "echo:" and the payload.
func udpEcho(t *testing.T) netip.AddrPort {
	conn := listenUDP(t)
	go func() {
		buf := make([]byte, 1500)
		for {
			n, from, err := conn.ReadFromUDPAddrPort(buf)
			if err != nil {
				return
			}
			conn.WriteToUDPAddrPort(append([]byte("echo:"), buf[:n]...), from)
		}
	}()
	return conn.LocalAddr().(*net.UDPAddr).AddrPort()
}

// A flow sends the destination's replies back through the reply socket for
// that destination, reuses one association per (src, dst), and ends after
// the idle timeout.
func TestUDPRelayFlows(t *testing.T) {
	echo := udpEcho(t)
	dsts := make(chan netip.AddrPort, 10)
	replies := make(chan netip.AddrPort, 10)
	openReply := func(dst netip.AddrPort) (net.PacketConn, error) {
		replies <- dst
		return net.ListenUDP("udp", &net.UDPAddr{IP: net.IPv4(127, 0, 0, 1)})
	}
	relay := newUDPRelay(loopbackDial(echo, dsts), openReply, 200*time.Millisecond)

	src := listenUDP(t)
	srcAddr := src.LocalAddr().(*net.UDPAddr).AddrPort()
	dst := netip.MustParseAddrPort("192.0.2.1:53")

	exchange := func(payload string) {
		t.Helper()
		relay.forward(srcAddr, dst, []byte(payload))
		src.SetReadDeadline(time.Now().Add(5 * time.Second))
		buf := make([]byte, 1500)
		n, err := src.Read(buf)
		if err != nil {
			t.Fatal(err)
		}
		if got, want := string(buf[:n]), "echo:"+payload; got != want {
			t.Fatalf("reply = %q, want %q", got, want)
		}
	}

	exchange("one")
	exchange("two")
	if got := <-dsts; got != dst {
		t.Errorf("dialed %v, want %v", got, dst)
	}
	if got := <-replies; got != dst {
		t.Errorf("reply socket bound to %v, want %v", got, dst)
	}
	if len(dsts) != 0 {
		t.Errorf("second packet dialed a new association")
	}

	deadline := time.Now().Add(5 * time.Second)
	for flowCount(relay) != 0 {
		if time.Now().After(deadline) {
			t.Fatal("idle flow was not removed")
		}
		time.Sleep(20 * time.Millisecond)
	}

	exchange("three")
	if len(dsts) != 1 {
		t.Errorf("packet after idle timeout reused the closed association")
	}
}

func flowCount(r *udpRelay) int {
	r.mu.Lock()
	defer r.mu.Unlock()
	return len(r.flows)
}

func TestCapturable(t *testing.T) {
	for addr, want := range map[string]bool{
		"192.0.2.1:443":           true,
		"[2001:db8::1]:53":        true,
		"127.0.0.1:443":           false,
		"[::1]:443":               false,
		"0.0.0.0:443":             false,
		"192.0.2.1:0":             false,
		"100.100.100.100:53":      false,
		"[fd7a:115c:a1e0::53]:53": false,
		"100.64.0.7:22":           true,
	} {
		if got := capturable(netip.MustParseAddrPort(addr)); got != want {
			t.Errorf("capturable(%s) = %v, want %v", addr, got, want)
		}
	}
}
