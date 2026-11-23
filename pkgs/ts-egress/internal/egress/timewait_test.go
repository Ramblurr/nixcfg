package egress

import (
	"bytes"
	"context"
	"io"
	"net/netip"
	"reflect"
	"testing"
	"time"
	"unsafe"

	"gvisor.dev/gvisor/pkg/tcpip"
	"gvisor.dev/gvisor/pkg/tcpip/adapters/gonet"
	"gvisor.dev/gvisor/pkg/tcpip/header"
	"gvisor.dev/gvisor/pkg/tcpip/network/ipv4"
	"gvisor.dev/gvisor/pkg/tcpip/stack"
	"gvisor.dev/gvisor/pkg/tcpip/transport/tcp"
	"tailscale.com/ipn"
	"tailscale.com/net/tsaddr"
	"tailscale.com/tsnet"
	"tailscale.com/wgengine/netstack"
)

// TestExitNodeReopensATimeWaitFlow closes a flow from the egress side, so
// the egress holds it in TIME_WAIT, then reconnects from the same tenant
// port, as a Linux client reusing an ephemeral port does. The egress drops
// a SYN for a tuple in TIME_WAIT, so the tenant connects only once TIME_WAIT
// ends and a SYN retransmit gets through; that must happen within the 5 s
// a client waits for its connection (oha's churn timeout).
func TestExitNodeReopensATimeWaitFlow(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()
	tw := newTimeWaitFlow(ctx, t)
	if err := tw.flow(); err != nil {
		t.Fatalf("a new SYN on the TIME_WAIT tuple: %v", err)
	}
}

// TestExitNodeKeepsATimeWaitFlowOnAStaleSYN replays the tenant's first SYN,
// an old duplicate whose sequence number is behind the closed flow's, on
// the tuple the egress holds in TIME_WAIT. The egress must drop it and
// stay in TIME_WAIT without a RST, so it still acks a retransmit of the
// tenant's last FIN.
func TestExitNodeKeepsATimeWaitFlowOnAStaleSYN(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()
	tw := newTimeWaitFlow(ctx, t)
	syn := tw.cap.first(func(b []byte) bool { return tw.fromTenant(b) && tcpFlags(b) == header.TCPFlagSyn })
	fin := tw.cap.last(func(b []byte) bool { return tw.fromTenant(b) && tcpFlags(b).Contains(header.TCPFlagFin) })
	if syn == nil || fin == nil {
		t.Fatal("captured no SYN and FIN from the tenant")
	}
	sent := tw.cap.mark()
	tun := tw.egress.Sys().Tun.Get()
	for _, p := range [][]byte{syn, fin} {
		if _, err := tun.Write([][]byte{bytes.Clone(p)}, 0); err != nil {
			t.Fatal(err)
		}
	}
	finAck := header.TCP(fin[header.IPv4(fin).HeaderLength():]).SequenceNumber() + 1
	acked := func(b []byte) bool {
		h := header.TCP(b[header.IPv4(b).HeaderLength():])
		return tw.toTenant(b) && h.Flags() == header.TCPFlagAck && h.AckNumber() == finAck
	}
	for deadline := time.Now().Add(5 * time.Second); tw.cap.since(sent, acked) == nil && time.Now().Before(deadline); {
		time.Sleep(20 * time.Millisecond)
	}
	if rst := tw.cap.since(sent, func(b []byte) bool { return tw.toTenant(b) && tcpFlags(b).Contains(header.TCPFlagRst) }); rst != nil {
		t.Fatalf("the stale SYN drew a RST: %v", tcpFlags(rst))
	}
	if st := endpointState(netstackOf(t, tw.egress), tw.tuple); st != tcp.StateTimeWait {
		t.Fatalf("the stale SYN ended TIME_WAIT: %v", st)
	}
	if tw.cap.since(sent, acked) == nil {
		t.Fatal("the egress never acked the retransmitted FIN")
	}
}

// timeWaitFlow is a flow from a tenant through the egress that the egress
// closed first and now holds in TIME_WAIT.
type timeWaitFlow struct {
	egress *tsnet.Server
	cap    *tcpCapture
	tuple  stack.TransportEndpointID // as the egress registers it
	flow   func() error              // opens the flow again from the same port
}

func (tw *timeWaitFlow) fromTenant(b []byte) bool { return tcpTuple(b) == tw.tuple }

func (tw *timeWaitFlow) toTenant(b []byte) bool {
	id := tcpTuple(b)
	return id.LocalAddress == tw.tuple.RemoteAddress && id.LocalPort == tw.tuple.RemotePort &&
		id.RemoteAddress == tw.tuple.LocalAddress && id.RemotePort == tw.tuple.LocalPort
}

func newTimeWaitFlow(ctx context.Context, t *testing.T) *timeWaitFlow {
	t.Helper()

	control := testControl(t)
	s, _ := newServer(nil, "")
	egress := tsnetNode(t, control, "egress", "tag:egress")
	if err := s.ExitNode(ctx, egress, time.Minute); err != nil {
		t.Fatal(err)
	}
	cap := captureTCP(t, egress)
	elc, err := egress.LocalClient()
	if err != nil {
		t.Fatal(err)
	}
	est, err := elc.Status(ctx)
	if err != nil {
		t.Fatal(err)
	}
	control.SetSubnetRoutes(est.Self.PublicKey, tsaddr.ExitRoutes())

	alice := tsnetNode(t, control, "alice", "tag:t3-alice")
	if _, err := alice.Up(ctx); err != nil {
		t.Fatal(err)
	}
	alc, err := alice.LocalClient()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := alc.EditPrefs(ctx, &ipn.MaskedPrefs{
		Prefs:         ipn.Prefs{ExitNodeID: control.Node(est.Self.PublicKey).StableID},
		ExitNodeIDSet: true,
	}); err != nil {
		t.Fatal(err)
	}
	for {
		st, err := alc.Status(ctx)
		if err != nil {
			t.Fatal(err)
		}
		if p := st.Peer[est.Self.PublicKey]; p != nil && p.ExitNode && p.PrimaryRoutes != nil && p.PrimaryRoutes.Len() > 0 {
			break
		}
		if ctx.Err() != nil {
			t.Fatal("alice never got the egress as its exit node")
		}
		time.Sleep(100 * time.Millisecond)
	}

	// alice has no grant, so the egress accepts the flow, denies it and
	// closes first: the egress is the active closer.
	aliceIP, _ := alice.TailscaleIPs()
	local := tcpip.FullAddress{Addr: tcpip.AddrFrom4(aliceIP.As4()), Port: 40000}
	dst := netip.MustParseAddrPort("198.51.100.5:7000")
	remote := tcpip.FullAddress{Addr: tcpip.AddrFrom4(dst.Addr().As4()), Port: dst.Port()}
	flow := func() error {
		dctx, cancel := context.WithTimeout(ctx, 5*time.Second)
		defer cancel()
		c, err := gonet.DialTCPWithBind(dctx, netstackOf(t, alice), local, remote, ipv4.ProtocolNumber)
		if err != nil {
			return err
		}
		defer c.Close()
		_ = c.SetReadDeadline(time.Now().Add(5 * time.Second))
		_, err = io.Copy(io.Discard, c)
		return err
	}
	if err := flow(); err != nil {
		t.Fatalf("first flow: %v", err)
	}

	tuple := stack.TransportEndpointID{
		LocalPort: remote.Port, LocalAddress: remote.Addr,
		RemotePort: local.Port, RemoteAddress: local.Addr,
	}
	for deadline := time.Now().Add(5 * time.Second); endpointState(netstackOf(t, egress), tuple) != tcp.StateTimeWait; time.Sleep(20 * time.Millisecond) {
		if time.Now().After(deadline) {
			t.Fatalf("the egress never held the flow in TIME_WAIT: %v", endpointState(netstackOf(t, egress), tuple))
		}
	}
	// The tenant, the passive closer, frees its port once the egress acks
	// its FIN.
	mine := stack.TransportEndpointID{
		LocalPort: local.Port, LocalAddress: local.Addr,
		RemotePort: remote.Port, RemoteAddress: remote.Addr,
	}
	for deadline := time.Now().Add(5 * time.Second); endpointState(netstackOf(t, alice), mine) != tcp.StateClose; time.Sleep(20 * time.Millisecond) {
		if time.Now().After(deadline) {
			t.Fatalf("the tenant never released its port: %v", endpointState(netstackOf(t, alice), mine))
		}
	}
	return &timeWaitFlow{egress: egress, cap: cap, tuple: tuple, flow: flow}
}

func netstackOf(t *testing.T, s *tsnet.Server) *stack.Stack {
	t.Helper()
	f := reflect.ValueOf(s.Sys().Netstack.Get().(*netstack.Impl)).Elem().FieldByName("ipstack")
	if !f.IsValid() {
		t.Fatal("netstack.Impl has no ipstack field")
	}
	return *(**stack.Stack)(unsafe.Pointer(f.UnsafeAddr()))
}

// endpointState returns the state of the TCP endpoint registered for id, or
// tcp.StateClose if there is none.
func endpointState(st *stack.Stack, id stack.TransportEndpointID) tcp.EndpointState {
	for _, ep := range st.RegisteredEndpoints() {
		if e, ok := ep.(*tcp.Endpoint); ok && e.Info().(*stack.TransportEndpointInfo).ID == id {
			return tcp.EndpointState(e.State())
		}
	}
	return tcp.StateClose
}
