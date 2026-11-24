package main

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"net/netip"
	"net/url"
	"strings"
	"testing"
	"time"

	"tailscale.com/net/tsaddr"
)

// Guest-originated dials reach the tailnet and never this node itself. In
// netstack a dial to the node's own address or to a Tailscale service IP is
// answered by the node's own handlers, such as PeerAPI and the quad-100
// resolver, with the node as its source.
func TestGuestDialsNeverReachTheNode(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()
	controlURL := startControl(t, nil).HTTPTestServer.URL

	plug, _ := startNode(t, ctx, controlURL, "plug")
	lc, err := plug.LocalClient()
	if err != nil {
		t.Fatal(err)
	}
	peer, peerIP := startNode(t, ctx, controlURL, "peer")
	waitForPeer(t, ctx, lc, peerIP)
	peerLn, err := peer.Listen("tcp", ":80")
	if err != nil {
		t.Fatal(err)
	}
	go http.Serve(peerLn, http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		io.WriteString(w, "peer")
	}))
	peerWeb := netip.AddrPortFrom(peerIP, 80)

	st, err := lc.Status(ctx)
	if err != nil {
		t.Fatal(err)
	}
	if len(st.Self.PeerAPIURL) == 0 {
		t.Fatal("plug has no PeerAPI")
	}
	nodeLocal := []netip.AddrPort{
		netip.AddrPortFrom(tsaddr.TailscaleServiceIP(), 53),
		netip.AddrPortFrom(tsaddr.TailscaleServiceIPv6(), 53),
		netip.AddrPortFrom(netip.AddrFrom16(tsaddr.TailscaleServiceIP().As16()), 53),
	}
	for _, u := range st.Self.PeerAPIURL {
		parsed, err := url.Parse(u)
		if err != nil {
			t.Fatal(err)
		}
		nodeLocal = append(nodeLocal, netip.MustParseAddrPort(parsed.Host))
	}

	dial, err := newNetstackDial(plug)
	if err != nil {
		t.Fatal(err)
	}
	proxy := httptest.NewServer(httpProxyHandler((&tailnetDialer{ts: plug, lc: lc, viaNetstack: dial}).Dial))
	defer proxy.Close()

	t.Run("proxy CONNECT", func(t *testing.T) {
		if got := connectStatus(t, proxy.Listener.Addr().String(), peerWeb); got != http.StatusOK {
			t.Errorf("CONNECT %v = %d, want 200", peerWeb, got)
		}
		for _, dst := range nodeLocal {
			if got := connectStatus(t, proxy.Listener.Addr().String(), dst); got == http.StatusOK {
				t.Errorf("CONNECT %v = 200, want refused", dst)
			}
		}
	})

	t.Run("proxy GET", func(t *testing.T) {
		proxyURL, _ := url.Parse(proxy.URL)
		client := &http.Client{Transport: &http.Transport{Proxy: http.ProxyURL(proxyURL)}, Timeout: 30 * time.Second}
		get := func(dst netip.AddrPort) (int, string) {
			resp, err := client.Get("http://" + dst.String() + "/")
			if err != nil {
				t.Fatalf("GET %v: %v", dst, err)
			}
			defer resp.Body.Close()
			body, _ := io.ReadAll(resp.Body)
			return resp.StatusCode, string(body)
		}
		if code, body := get(peerWeb); code != http.StatusOK || body != "peer" {
			t.Errorf("GET %v = %d %q, want 200 \"peer\"", peerWeb, code, body)
		}
		for _, u := range st.Self.PeerAPIURL {
			dst := netip.MustParseAddrPort(strings.TrimPrefix(u, "http://"))
			if code, body := get(dst); code != http.StatusBadGateway {
				t.Errorf("GET PeerAPI %v = %d %q, want 502", dst, code, body)
			}
		}
	})

	t.Run("transparent TCP", func(t *testing.T) {
		if got := transparentGET(t, dial, peerWeb); !strings.HasPrefix(got, "HTTP/1.1 200") {
			t.Errorf("captured GET %v = %q, want 200", peerWeb, got)
		}
		for _, dst := range nodeLocal {
			if got := transparentGET(t, dial, dst); got != "" {
				t.Errorf("captured GET %v = %q, want no reply", dst, got)
			}
		}
	})

	t.Run("transparent UDP", func(t *testing.T) {
		ip4, ip6 := plug.TailscaleIPs()
		self4, err := plug.ListenPacket("udp", netip.AddrPortFrom(ip4, 5353).String())
		if err != nil {
			t.Fatal(err)
		}
		defer self4.Close()
		self6, err := plug.ListenPacket("udp", netip.AddrPortFrom(ip6, 5353).String())
		if err != nil {
			t.Fatal(err)
		}
		defer self6.Close()
		remote, err := peer.ListenPacket("udp", netip.AddrPortFrom(peerIP, 5353).String())
		if err != nil {
			t.Fatal(err)
		}
		defer remote.Close()

		relay := newUDPRelay(dial, func(netip.AddrPort) (net.PacketConn, error) {
			return net.ListenUDP("udp", &net.UDPAddr{IP: net.IPv4(127, 0, 0, 1)})
		}, time.Minute)
		src := netip.MustParseAddrPort("127.0.0.1:40000")
		if !udpArrives(relay, src, netip.AddrPortFrom(peerIP, 5353), remote) {
			t.Errorf("captured UDP to peer %v did not arrive", peerIP)
		}
		for _, l := range []net.PacketConn{self4, self6} {
			dst := l.LocalAddr().(*net.UDPAddr).AddrPort()
			if udpArrives(relay, src, dst, l) {
				t.Errorf("captured UDP to the node's own %v arrived", dst)
			}
		}
	})
}

// connectStatus sends CONNECT dst to the HTTP proxy at proxyAddr and returns
// the response status.
func connectStatus(t *testing.T, proxyAddr string, dst netip.AddrPort) int {
	t.Helper()
	conn, err := net.Dial("tcp", proxyAddr)
	if err != nil {
		t.Fatal(err)
	}
	defer conn.Close()
	conn.SetDeadline(time.Now().Add(30 * time.Second))
	fmt.Fprintf(conn, "CONNECT %s HTTP/1.1\r\nHost: %s\r\n\r\n", dst, dst)
	resp, err := http.ReadResponse(bufio.NewReader(conn), &http.Request{Method: http.MethodConnect})
	if err != nil {
		t.Fatalf("CONNECT %v: %v", dst, err)
	}
	return resp.StatusCode
}

// transparentGET hands a client connection captured on its way to dst to the
// transparent TCP path, sends a GET and returns whatever comes back.
func transparentGET(t *testing.T, dial netstackDial, dst netip.AddrPort) string {
	t.Helper()
	ln, err := net.ListenTCP("tcp", &net.TCPAddr{IP: net.IPv4(127, 0, 0, 1)})
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	go func() {
		conn, err := ln.AcceptTCP()
		if err != nil {
			return
		}
		proxyTransparentTCP(conn, dst, dial)
	}()
	client, err := net.Dial("tcp", ln.Addr().String())
	if err != nil {
		t.Fatal(err)
	}
	defer client.Close()
	client.SetDeadline(time.Now().Add(30 * time.Second))
	fmt.Fprintf(client, "GET / HTTP/1.1\r\nHost: %s\r\nConnection: close\r\n\r\n", dst)
	reply, _ := io.ReadAll(client)
	return string(reply)
}

// udpArrives relays a datagram captured from src on its way to dst and
// reports whether l, listening at dst, receives it.
func udpArrives(relay *udpRelay, src, dst netip.AddrPort, l net.PacketConn) bool {
	relay.forward(src, dst, []byte("ping"))
	l.SetReadDeadline(time.Now().Add(2 * time.Second))
	buf := make([]byte, 16)
	n, _, err := l.ReadFrom(buf)
	return err == nil && string(buf[:n]) == "ping"
}
