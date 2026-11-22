package main

import (
	"bufio"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"net"
	"net/netip"
	"os"
	"os/exec"
	"strings"
	"syscall"
	"testing"
	"time"
	"unsafe"

	"golang.org/x/sys/unix"
)

func TestParseSockaddr(t *testing.T) {
	v4 := make([]byte, unix.SizeofSockaddrInet4)
	binary.NativeEndian.PutUint16(v4, unix.AF_INET)
	binary.BigEndian.PutUint16(v4[2:], 443)
	copy(v4[4:], []byte{192, 0, 2, 7})

	v6 := make([]byte, unix.SizeofSockaddrInet6)
	binary.NativeEndian.PutUint16(v6, unix.AF_INET6)
	binary.BigEndian.PutUint16(v6[2:], 53)
	copy(v6[8:], netip.MustParseAddr("2001:db8::9").AsSlice())

	mapped := make([]byte, unix.SizeofSockaddrInet6)
	copy(mapped, v6)
	copy(mapped[8:], netip.MustParseAddr("::ffff:192.0.2.8").AsSlice())

	for name, tc := range map[string]struct {
		in   []byte
		want string
	}{
		"v4":        {v4, "192.0.2.7:443"},
		"v6":        {v6, "[2001:db8::9]:53"},
		"v4-mapped": {mapped, "192.0.2.8:53"},
	} {
		got, err := parseSockaddr(tc.in)
		if err != nil {
			t.Errorf("%s: %v", name, err)
			continue
		}
		if got.String() != tc.want {
			t.Errorf("%s: got %v, want %s", name, got, tc.want)
		}
	}
	for name, in := range map[string][]byte{
		"empty":      nil,
		"short v4":   v4[:8],
		"short v6":   v6[:20],
		"bad family": {0xff, 0xff, 0, 0},
	} {
		if _, err := parseSockaddr(in); err == nil {
			t.Errorf("%s: no error", name)
		}
	}
}

func TestUDPOriginalDst(t *testing.T) {
	sa := make([]byte, unix.SizeofSockaddrInet4)
	binary.NativeEndian.PutUint16(sa, unix.AF_INET)
	binary.BigEndian.PutUint16(sa[2:], 53)
	copy(sa[4:], []byte{198, 51, 100, 1})

	oob := make([]byte, unix.CmsgSpace(len(sa)))
	h := (*unix.Cmsghdr)(unsafe.Pointer(&oob[0]))
	h.Level = unix.SOL_IP
	h.Type = unix.IP_ORIGDSTADDR
	h.SetLen(unix.CmsgLen(len(sa)))
	copy(oob[unix.CmsgLen(0):], sa)

	got, err := udpOriginalDst(oob)
	if err != nil {
		t.Fatal(err)
	}
	if want := netip.MustParseAddrPort("198.51.100.1:53"); got != want {
		t.Errorf("got %v, want %v", got, want)
	}
	if _, err := udpOriginalDst(nil); err == nil {
		t.Error("no control message: no error")
	}
}

const netnsTestEnv = "TSPLUG_NETNS_TEST"

// TestTransparentNetns captures real traffic with nftables in a user and
// network namespace, so it needs no root. It checks TPROXY for TCP and UDP
// and REDIRECT for TCP, with the rules the README documents.
func TestTransparentNetns(t *testing.T) {
	if os.Getenv(netnsTestEnv) != "1" {
		for _, tool := range []string{"unshare", "ip", "nft"} {
			if _, err := exec.LookPath(tool); err != nil {
				t.Skipf("%s not found", tool)
			}
		}
		if err := exec.Command("unshare", "-Urn", "true").Run(); err != nil {
			t.Skipf("cannot create a user and network namespace: %v", err)
		}
		cmd := exec.Command("unshare", "-Urn", os.Args[0], "-test.run=^TestTransparentNetns$", "-test.v")
		cmd.Env = append(os.Environ(), netnsTestEnv+"=1")
		out, err := cmd.CombinedOutput()
		t.Logf("in namespace:\n%s", out)
		if err != nil {
			t.Fatal(err)
		}
		if strings.Contains(string(out), "--- SKIP") {
			t.Skip("skipped in namespace")
		}
		return
	}

	run := func(args ...string) {
		t.Helper()
		if out, err := exec.Command(args[0], args[1:]...).CombinedOutput(); err != nil {
			t.Fatalf("%s: %v\n%s", strings.Join(args, " "), err, out)
		}
	}
	run("ip", "link", "set", "lo", "up")
	run("ip", "link", "add", "d0", "type", "dummy")
	run("ip", "addr", "add", "10.9.0.1/24", "dev", "d0")
	run("ip", "link", "set", "d0", "up")
	run("ip", "route", "add", "default", "dev", "d0")
	run("ip", "rule", "add", "fwmark", "0x1", "lookup", "100")
	run("ip", "route", "add", "local", "0.0.0.0/0", "dev", "lo", "table", "100")
	// The TPROXY ruleset from docs/ts-plug.md, with the ts-plug uid
	// unmapped here, plus a REDIRECT rule for 198.51.100.0/24 that the
	// TPROXY rules skip.
	rules := `
table inet tsplug {
	chain output {
		type route hook output priority mangle; policy accept;
		meta skuid 1234 return
		fib daddr type local return
		ip daddr 198.51.100.0/24 return
		meta nfproto ipv6 reject
		meta l4proto { tcp, udp } meta mark set 0x1
	}
	chain prerouting {
		type filter hook prerouting priority mangle; policy accept;
		iifname != "lo" return
		meta mark 0x1 meta l4proto { tcp, udp } tproxy ip to 127.0.0.1:15001 accept
		meta mark 0x1 meta l4proto tcp reject with tcp reset
		meta mark 0x1 reject
	}
	chain nat-output {
		type nat hook output priority dstnat; policy accept;
		meta skuid 1234 return
		ip daddr 198.51.100.0/24 tcp dport 80 redirect to :15002
	}
}`
	nft := exec.Command("nft", "-f", "-")
	nft.Stdin = strings.NewReader(rules)
	if out, err := nft.CombinedOutput(); err != nil {
		t.Skipf("nftables TPROXY rules unavailable: %v\n%s", err, out)
	}

	// With ts-plug not listening, captured TCP is refused at once.
	conn, err := net.DialTimeout("tcp", "203.0.113.10:443", 5*time.Second)
	if err == nil {
		conn.Close()
		t.Fatal("captured TCP connected with no transparent listener")
	}
	if !errors.Is(err, syscall.ECONNREFUSED) {
		t.Fatalf("captured TCP with no listener: %v, want connection refused", err)
	}

	tcpBackend := tcpReplyAfterEOF(t)
	udpBackend := udpEcho(t)
	dsts := make(chan netip.AddrPort, 10)
	fail := func(err error) { t.Error(err) }
	tcpDial := loopbackDial(tcpBackend, dsts)
	if err := startTransparentTCP("127.0.0.1:15001", tcpDial, fail); err != nil {
		t.Fatal(err)
	}
	if err := startTransparentTCP("127.0.0.1:15002", tcpDial, fail); err != nil {
		t.Fatal(err)
	}
	if err := startTransparentUDP("127.0.0.1:15001", loopbackDial(udpBackend, dsts), fail); err != nil {
		t.Fatal(err)
	}

	for _, dst := range []string{"203.0.113.10:443", "198.51.100.20:80"} {
		t.Run("tcp "+dst, func(t *testing.T) {
			conn, err := net.DialTimeout("tcp", dst, 5*time.Second)
			if err != nil {
				t.Fatal(err)
			}
			defer conn.Close()
			conn.SetDeadline(time.Now().Add(5 * time.Second))
			io.WriteString(conn, "hello")
			conn.(*net.TCPConn).CloseWrite()
			reply, err := io.ReadAll(conn)
			if err != nil {
				t.Fatal(err)
			}
			if got, want := string(reply), "got:hello"; got != want {
				t.Errorf("reply = %q, want %q", got, want)
			}
			if got := <-dsts; got.String() != dst {
				t.Errorf("dialed %v, want %s", got, dst)
			}
		})
	}

	t.Run("udp", func(t *testing.T) {
		dst := "203.0.113.53:53"
		conn, err := net.Dial("udp", dst)
		if err != nil {
			t.Fatal(err)
		}
		defer conn.Close()
		conn.SetDeadline(time.Now().Add(5 * time.Second))
		for _, payload := range []string{"query1", "query2"} {
			io.WriteString(conn, payload)
			buf := make([]byte, 1500)
			n, err := conn.Read(buf)
			if err != nil {
				t.Fatal(err)
			}
			// A connected socket accepts only datagrams from dst, so this
			// reply proves the transparent reply socket spoofed dst.
			if got, want := string(buf[:n]), "echo:"+payload; got != want {
				t.Errorf("reply = %q, want %q", got, want)
			}
		}
		if got := <-dsts; got.String() != dst {
			t.Errorf("dialed %v, want %s", got, dst)
		}
		if len(dsts) != 0 {
			t.Errorf("second datagram opened a second flow")
		}
	})
}

// tcpReplyAfterEOF answers each connection with "got:" and everything it
// read, after the client half-closes.
func tcpReplyAfterEOF(t *testing.T) netip.AddrPort {
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { ln.Close() })
	go func() {
		for {
			conn, err := ln.Accept()
			if err != nil {
				return
			}
			go func() {
				defer conn.Close()
				request, _ := io.ReadAll(bufio.NewReader(conn))
				fmt.Fprintf(conn, "got:%s", request)
			}()
		}
	}()
	return ln.Addr().(*net.TCPAddr).AddrPort()
}
