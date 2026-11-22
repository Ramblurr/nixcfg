package egress

import (
	"context"
	"encoding/binary"
	"errors"
	"net"
	"net/netip"
	"syscall"

	"golang.org/x/sys/unix"
)

// transparent marks a socket IP_TRANSPARENT so it accepts TPROXY'd traffic
// for foreign addresses and may bind to them.
func transparent(network string, c syscall.RawConn, extra ...func(fd int) error) error {
	var serr error
	err := c.Control(func(fd uintptr) {
		level, opt := unix.SOL_IP, unix.IP_TRANSPARENT
		if network == "tcp6" || network == "udp6" {
			level, opt = unix.SOL_IPV6, unix.IPV6_TRANSPARENT
		}
		if serr = unix.SetsockoptInt(int(fd), level, opt, 1); serr != nil {
			return
		}
		for _, f := range extra {
			if serr = f(int(fd)); serr != nil {
				return
			}
		}
	})
	return errors.Join(err, serr)
}

// ListenTCP opens a transparent TCP listener. Accepted connections carry the
// client's original destination as their local address.
func ListenTCP(ctx context.Context, network, addr string) (net.Listener, error) {
	lc := net.ListenConfig{Control: func(network, _ string, c syscall.RawConn) error {
		return transparent(network, c)
	}}
	return lc.Listen(ctx, network, addr)
}

// ListenUDP opens a transparent UDP socket that reports each datagram's
// original destination in its control messages.
func ListenUDP(ctx context.Context, network, addr string) (*net.UDPConn, error) {
	lc := net.ListenConfig{Control: func(network, _ string, c syscall.RawConn) error {
		return transparent(network, c, func(fd int) error {
			if network == "udp6" {
				return unix.SetsockoptInt(fd, unix.SOL_IPV6, unix.IPV6_RECVORIGDSTADDR, 1)
			}
			return unix.SetsockoptInt(fd, unix.SOL_IP, unix.IP_RECVORIGDSTADDR, 1)
		})
	}}
	pc, err := lc.ListenPacket(ctx, network, addr)
	if err != nil {
		return nil, err
	}
	return pc.(*net.UDPConn), nil
}

// origDst extracts IP_ORIGDSTADDR or IPV6_ORIGDSTADDR from oob.
func origDst(oob []byte) (netip.AddrPort, bool) {
	msgs, err := unix.ParseSocketControlMessage(oob)
	if err != nil {
		return netip.AddrPort{}, false
	}
	for _, m := range msgs {
		switch {
		case m.Header.Level == unix.SOL_IP && m.Header.Type == unix.IP_ORIGDSTADDR && len(m.Data) >= unix.SizeofSockaddrInet4:
			port := binary.BigEndian.Uint16(m.Data[2:4])
			return netip.AddrPortFrom(netip.AddrFrom4([4]byte(m.Data[4:8])), port), true
		case m.Header.Level == unix.SOL_IPV6 && m.Header.Type == unix.IPV6_ORIGDSTADDR && len(m.Data) >= unix.SizeofSockaddrInet6:
			port := binary.BigEndian.Uint16(m.Data[2:4])
			return netip.AddrPortFrom(netip.AddrFrom16([16]byte(m.Data[8:24])).Unmap(), port), true
		}
	}
	return netip.AddrPort{}, false
}

// replyConn opens a UDP socket bound to the client's original destination
// and connected to the client, so replies appear to come from the server.
func replyConn(ctx context.Context, from, to netip.AddrPort) (*net.UDPConn, error) {
	network := "udp4"
	if from.Addr().Is6() {
		network = "udp6"
	}
	d := net.Dialer{
		LocalAddr: net.UDPAddrFromAddrPort(from),
		Control: func(network, _ string, c syscall.RawConn) error {
			return transparent(network, c, func(fd int) error {
				return unix.SetsockoptInt(fd, unix.SOL_SOCKET, unix.SO_REUSEADDR, 1)
			})
		},
	}
	c, err := d.DialContext(ctx, network, to.String())
	if err != nil {
		return nil, err
	}
	return c.(*net.UDPConn), nil
}
