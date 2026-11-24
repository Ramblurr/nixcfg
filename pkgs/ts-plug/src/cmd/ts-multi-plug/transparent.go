package main

// Transparent capture. The namespace's nftables steers local TCP and UDP
// into these listeners, and each flow goes to its original destination over
// the tailnet: straight to a tailnet peer, or through the exit node.

import (
	"context"
	"errors"
	"log/slog"
	"net"
	"net/netip"
	"sync"
	"sync/atomic"
	"time"
)

const (
	transparentDialTimeout = 30 * time.Second
	udpFlowIdleTimeout     = time.Minute
)

func proxyTransparentTCP(client *net.TCPConn, dst netip.AddrPort, dial netstackDial) {
	defer client.Close()
	ctx, cancel := context.WithTimeout(context.Background(), transparentDialTimeout)
	backend, err := dial(ctx, "tcp", dst)
	cancel()
	if err != nil {
		slog.Info("transparent TCP: dial failed", slog.String("dst", dst.String()), slog.Any("error", err))
		client.SetLinger(0) // close with a reset, not an empty stream
		return
	}
	defer backend.Close()
	splice(client, client, backend)
}

type udpFlowKey struct {
	src, dst netip.AddrPort
}

// udpFlow is one source's association with one original destination.
type udpFlow struct {
	upstream net.Conn       // connected to dst over the tailnet
	reply    net.PacketConn // bound to dst, so replies reach src from dst
	lastSeen atomic.Int64   // UnixNano of the last packet either way
}

func (f *udpFlow) touch() { f.lastSeen.Store(time.Now().UnixNano()) }

func (f *udpFlow) idleSince() time.Time { return time.Unix(0, f.lastSeen.Load()) }

// udpRelay relays captured UDP datagrams. A flow ends after idle passes with
// no packet either way.
type udpRelay struct {
	dial      netstackDial
	openReply func(dst netip.AddrPort) (net.PacketConn, error)
	idle      time.Duration

	mu    sync.Mutex
	flows map[udpFlowKey]*udpFlow
}

func newUDPRelay(dial netstackDial, openReply func(netip.AddrPort) (net.PacketConn, error), idle time.Duration) *udpRelay {
	return &udpRelay{dial: dial, openReply: openReply, idle: idle, flows: map[udpFlowKey]*udpFlow{}}
}

// forward sends payload, captured from src on its way to dst, to dst.
func (r *udpRelay) forward(src, dst netip.AddrPort, payload []byte) {
	flow, err := r.flow(udpFlowKey{src: src, dst: dst})
	if err != nil {
		slog.Info("transparent UDP: open flow failed", slog.String("src", src.String()), slog.String("dst", dst.String()), slog.Any("error", err))
		return
	}
	flow.touch()
	if _, err := flow.upstream.Write(payload); err != nil {
		slog.Debug("transparent UDP: write upstream failed", slog.String("dst", dst.String()), slog.Any("error", err))
	}
}

func (r *udpRelay) flow(key udpFlowKey) (*udpFlow, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if flow, ok := r.flows[key]; ok {
		return flow, nil
	}
	ctx, cancel := context.WithTimeout(context.Background(), transparentDialTimeout)
	defer cancel()
	upstream, err := r.dial(ctx, "udp", key.dst)
	if err != nil {
		return nil, err
	}
	reply, err := r.openReply(key.dst)
	if err != nil {
		upstream.Close()
		return nil, err
	}
	flow := &udpFlow{upstream: upstream, reply: reply}
	flow.touch()
	r.flows[key] = flow
	go r.returnReplies(key, flow)
	return flow, nil
}

// returnReplies sends dst's replies back to src until the flow idles out,
// then closes and forgets the flow.
func (r *udpRelay) returnReplies(key udpFlowKey, flow *udpFlow) {
	defer r.remove(key, flow)
	srcAddr := net.UDPAddrFromAddrPort(key.src)
	buf := make([]byte, 64<<10)
	for {
		flow.upstream.SetReadDeadline(flow.idleSince().Add(r.idle))
		n, err := flow.upstream.Read(buf)
		if err != nil {
			var netErr net.Error
			if errors.As(err, &netErr) && netErr.Timeout() && time.Since(flow.idleSince()) < r.idle {
				continue // the source sent a packet since the deadline was set
			}
			return
		}
		flow.touch()
		if _, err := flow.reply.WriteTo(buf[:n], srcAddr); err != nil {
			slog.Debug("transparent UDP: reply failed", slog.String("src", key.src.String()), slog.Any("error", err))
		}
	}
}

func (r *udpRelay) remove(key udpFlowKey, flow *udpFlow) {
	r.mu.Lock()
	if r.flows[key] == flow {
		delete(r.flows, key)
	}
	r.mu.Unlock()
	flow.upstream.Close()
	flow.reply.Close()
}
