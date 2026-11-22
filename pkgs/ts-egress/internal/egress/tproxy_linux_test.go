package egress

import (
	"context"
	"net"
	"net/netip"
	"os"
	"testing"

	"golang.org/x/sys/unix"
)

// IP_RECVORIGDSTADDR needs no privilege, so the original-destination parse
// runs everywhere. Without TPROXY the original destination is the socket's
// own address.
func TestOrigDstFromControlMessage(t *testing.T) {
	for _, tt := range []struct{ network, addr string }{
		{"udp4", "127.0.0.1:0"},
		{"udp6", "[::1]:0"},
	} {
		pc, err := net.ListenPacket(tt.network, tt.addr)
		if err != nil {
			t.Skipf("%s: %v", tt.network, err)
		}
		uc := pc.(*net.UDPConn)
		raw, _ := uc.SyscallConn()
		_ = raw.Control(func(fd uintptr) {
			if tt.network == "udp6" {
				err = unix.SetsockoptInt(int(fd), unix.SOL_IPV6, unix.IPV6_RECVORIGDSTADDR, 1)
			} else {
				err = unix.SetsockoptInt(int(fd), unix.SOL_IP, unix.IP_RECVORIGDSTADDR, 1)
			}
		})
		if err != nil {
			t.Fatal(err)
		}
		c, err := net.Dial(tt.network, uc.LocalAddr().String())
		if err != nil {
			t.Fatal(err)
		}
		_, _ = c.Write([]byte("x"))
		buf, oob := make([]byte, 16), make([]byte, 256)
		_, oobn, _, _, err := uc.ReadMsgUDPAddrPort(buf, oob)
		if err != nil {
			t.Fatal(err)
		}
		got, ok := origDst(oob[:oobn])
		want := uc.LocalAddr().(*net.UDPAddr).AddrPort()
		if !ok || got != netip.AddrPortFrom(want.Addr().Unmap(), want.Port()) {
			t.Fatalf("%s: origDst = %v %v, want %v", tt.network, got, ok, want)
		}
		c.Close()
		uc.Close()
	}
}

// Transparent sockets need CAP_NET_ADMIN. The TPROXY rules themselves are
// exercised on the egress host (108-03).
func TestTransparentListenersAsRoot(t *testing.T) {
	if os.Geteuid() != 0 {
		t.Skip("needs root for IP_TRANSPARENT")
	}
	ln, err := ListenTCP(context.Background(), "tcp4", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	ln.Close()
	pc, err := ListenUDP(context.Background(), "udp4", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	pc.Close()
	rc, err := replyConn(context.Background(), netip.MustParseAddrPort("192.0.2.1:53"), netip.MustParseAddrPort("127.0.0.1:9"))
	if err != nil {
		t.Fatalf("reply socket bound to a foreign address: %v", err)
	}
	rc.Close()
}
