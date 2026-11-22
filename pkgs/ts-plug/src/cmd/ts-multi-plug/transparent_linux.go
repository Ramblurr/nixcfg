package main

import (
	"cmp"
	"context"
	"encoding/binary"
	"errors"
	"fmt"
	"log/slog"
	"net"
	"net/netip"
	"strings"
	"syscall"
	"unsafe"

	"golang.org/x/sys/unix"
)

// startTransparentTCP accepts TCP that nftables steers to addr with TPROXY
// or REDIRECT and dials each connection's original destination.
func startTransparentTCP(addr string, dial netstackDial, fail func(error)) error {
	lc := net.ListenConfig{Control: func(network, _ string, c syscall.RawConn) error {
		return setsockopts(c, transparentOpts(network)...)
	}}
	ln, err := lc.Listen(context.Background(), "tcp", addr)
	if err != nil {
		return fmt.Errorf("listen for transparent TCP on %s: %w", addr, err)
	}
	slog.Info("transparent TCP listening", slog.String("local", ln.Addr().String()))
	go func() {
		for {
			conn, err := ln.Accept()
			if err != nil {
				fail(fmt.Errorf("transparent TCP listener stopped: %w", err))
				return
			}
			tcp := conn.(*net.TCPConn)
			dst, err := tcpOriginalDst(tcp)
			if err != nil {
				slog.Warn("transparent TCP: no original destination", slog.Any("error", err))
				tcp.Close()
				continue
			}
			go proxyTransparentTCP(tcp, dst, dial)
		}
	}()
	return nil
}

// startTransparentUDP receives UDP that nftables steers to addr with TPROXY
// and relays each flow to its original destination.
func startTransparentUDP(addr string, dial netstackDial, fail func(error)) error {
	lc := net.ListenConfig{Control: func(network, _ string, c syscall.RawConn) error {
		opts := append(transparentOpts(network), sockopt{unix.SOL_IP, unix.IP_RECVORIGDSTADDR})
		if strings.HasSuffix(network, "6") {
			opts = append(opts, sockopt{unix.SOL_IPV6, unix.IPV6_RECVORIGDSTADDR})
		}
		return setsockopts(c, opts...)
	}}
	pc, err := lc.ListenPacket(context.Background(), "udp", addr)
	if err != nil {
		return fmt.Errorf("listen for transparent UDP on %s: %w", addr, err)
	}
	conn := pc.(*net.UDPConn)
	slog.Info("transparent UDP listening", slog.String("local", conn.LocalAddr().String()))
	relay := newUDPRelay(dial, listenTransparentReply, udpFlowIdleTimeout)
	go func() {
		buf := make([]byte, 64<<10)
		oob := make([]byte, 512)
		for {
			n, oobn, _, src, err := conn.ReadMsgUDPAddrPort(buf, oob)
			if err != nil {
				fail(fmt.Errorf("transparent UDP listener stopped: %w", err))
				return
			}
			dst, err := udpOriginalDst(oob[:oobn])
			if err != nil {
				slog.Warn("transparent UDP: no original destination", slog.String("src", src.String()), slog.Any("error", err))
				continue
			}
			relay.forward(unmap(src), dst, buf[:n])
		}
	}()
	return nil
}

// listenTransparentReply opens an unconnected socket bound to dst, which
// need not be a local address. It must stay unconnected: TPROXY delivers a
// packet to a connected socket matching its addresses before the listener,
// so a connected reply socket would swallow the source's later packets.
// SO_REUSEADDR lets flows from several sources to one dst share the address.
func listenTransparentReply(dst netip.AddrPort) (net.PacketConn, error) {
	network := "udp4"
	if dst.Addr().Is6() {
		network = "udp6"
	}
	lc := net.ListenConfig{Control: func(network, _ string, c syscall.RawConn) error {
		return setsockopts(c, append(transparentOpts(network), sockopt{unix.SOL_SOCKET, unix.SO_REUSEADDR})...)
	}}
	return lc.ListenPacket(context.Background(), network, dst.String())
}

// ip6tSOOriginalDst is IP6T_SO_ORIGINAL_DST from linux/netfilter_ipv6/ip6_tables.h.
const ip6tSOOriginalDst = 80

// tcpOriginalDst returns where conn's client was connecting. With REDIRECT
// that is SO_ORIGINAL_DST. TPROXY keeps it as the local address, and
// SO_ORIGINAL_DST then fails without conntrack.
func tcpOriginalDst(conn *net.TCPConn) (netip.AddrPort, error) {
	local := unmap(conn.LocalAddr().(*net.TCPAddr).AddrPort())
	level, name := unix.SOL_IP, unix.SO_ORIGINAL_DST
	if local.Addr().Is6() {
		level, name = unix.SOL_IPV6, ip6tSOOriginalDst
	}
	raw, err := conn.SyscallConn()
	if err != nil {
		return netip.AddrPort{}, err
	}
	var dst netip.AddrPort
	var optErr error
	if err := raw.Control(func(fd uintptr) {
		dst, optErr = getsockoptSockaddr(int(fd), level, name)
	}); err != nil {
		return netip.AddrPort{}, err
	}
	if optErr != nil {
		return local, nil
	}
	return dst, nil
}

// udpOriginalDst returns the original destination from a datagram's
// IP_ORIGDSTADDR or IPV6_ORIGDSTADDR control message.
func udpOriginalDst(oob []byte) (netip.AddrPort, error) {
	msgs, err := unix.ParseSocketControlMessage(oob)
	if err != nil {
		return netip.AddrPort{}, err
	}
	for _, msg := range msgs {
		if (msg.Header.Level == unix.SOL_IP && msg.Header.Type == unix.IP_ORIGDSTADDR) ||
			(msg.Header.Level == unix.SOL_IPV6 && msg.Header.Type == unix.IPV6_ORIGDSTADDR) {
			return parseSockaddr(msg.Data)
		}
	}
	return netip.AddrPort{}, errors.New("no IP_ORIGDSTADDR control message")
}

func getsockoptSockaddr(fd, level, name int) (netip.AddrPort, error) {
	var buf [unix.SizeofSockaddrInet6]byte
	size := uint32(len(buf))
	_, _, errno := unix.Syscall6(unix.SYS_GETSOCKOPT, uintptr(fd), uintptr(level), uintptr(name),
		uintptr(unsafe.Pointer(&buf[0])), uintptr(unsafe.Pointer(&size)), 0)
	if errno != 0 {
		return netip.AddrPort{}, errno
	}
	return parseSockaddr(buf[:size])
}

// parseSockaddr parses a raw sockaddr_in or sockaddr_in6.
func parseSockaddr(b []byte) (netip.AddrPort, error) {
	if len(b) < 2 {
		return netip.AddrPort{}, fmt.Errorf("sockaddr too short: %d bytes", len(b))
	}
	switch binary.NativeEndian.Uint16(b) {
	case unix.AF_INET:
		if len(b) < unix.SizeofSockaddrInet4 {
			return netip.AddrPort{}, fmt.Errorf("sockaddr_in too short: %d bytes", len(b))
		}
		return netip.AddrPortFrom(netip.AddrFrom4([4]byte(b[4:8])), binary.BigEndian.Uint16(b[2:4])), nil
	case unix.AF_INET6:
		if len(b) < unix.SizeofSockaddrInet6 {
			return netip.AddrPort{}, fmt.Errorf("sockaddr_in6 too short: %d bytes", len(b))
		}
		addr := netip.AddrFrom16([16]byte(b[8:24])).Unmap()
		return netip.AddrPortFrom(addr, binary.BigEndian.Uint16(b[2:4])), nil
	default:
		return netip.AddrPort{}, fmt.Errorf("unsupported address family %d", binary.NativeEndian.Uint16(b))
	}
}

type sockopt struct{ level, name int }

// transparentOpts lets a socket receive TPROXY traffic and bind to
// addresses that are not local.
func transparentOpts(network string) []sockopt {
	opts := []sockopt{{unix.SOL_IP, unix.IP_TRANSPARENT}}
	if strings.HasSuffix(network, "6") {
		opts = append(opts, sockopt{unix.SOL_IPV6, unix.IPV6_TRANSPARENT})
	}
	return opts
}

func setsockopts(c syscall.RawConn, opts ...sockopt) error {
	var optErr error
	err := c.Control(func(fd uintptr) {
		for _, opt := range opts {
			if err := unix.SetsockoptInt(int(fd), opt.level, opt.name, 1); err != nil {
				optErr = fmt.Errorf("setsockopt(%d, %d): %w", opt.level, opt.name, err)
				return
			}
		}
	})
	return cmp.Or(err, optErr)
}
