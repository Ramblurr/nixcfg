package egress

import (
	"bytes"
	"context"
	"testing"
	"time"

	"gvisor.dev/gvisor/pkg/tcpip/header"
	"gvisor.dev/gvisor/pkg/tcpip/stack"
	"gvisor.dev/gvisor/pkg/tcpip/transport/tcp"
)

// Two copies of a reopening SYN queued on the TIME_WAIT endpoint still open
// one connection: the second copy must not draw a reset that refuses the
// tenant's dial (review case on google/gvisor#15019).
func TestExitNodeReopensATimeWaitFlowOnADuplicatedSYN(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()
	tw := newTimeWaitFlow(ctx, t)
	var ep *tcp.Endpoint
	for _, e := range netstackOf(t, tw.egress).RegisteredEndpoints() {
		if te, ok := e.(*tcp.Endpoint); ok && te.Info().(*stack.TransportEndpointInfo).ID == tw.tuple {
			ep = te
		}
	}
	if ep == nil {
		t.Fatal("no TIME_WAIT endpoint")
	}
	isSYN := func(b []byte) bool { return tw.fromTenant(b) && tcpFlags(b) == header.TCPFlagSyn }
	sent := tw.cap.mark()
	ep.LockUser()
	first := tw.cap.last(isSYN)
	done := make(chan error, 1)
	go func() { done <- tw.flow() }()
	var syn []byte
	for deadline := time.Now().Add(5 * time.Second); syn == nil || bytes.Equal(syn, first); syn = tw.cap.last(isSYN) {
		if time.Now().After(deadline) {
			ep.UnlockUser()
			t.Fatal("the tenant never sent a new SYN")
		}
		time.Sleep(10 * time.Millisecond)
	}
	if _, err := tw.egress.Sys().Tun.Get().Write([][]byte{bytes.Clone(syn)}, 0); err != nil {
		ep.UnlockUser()
		t.Fatal(err)
	}
	time.Sleep(100 * time.Millisecond)
	ep.UnlockUser()
	err := <-done
	rst := tw.cap.since(sent, func(b []byte) bool { return tw.toTenant(b) && tcpFlags(b).Contains(header.TCPFlagRst) })
	t.Logf("dial err=%v rst=%v", err, rst != nil)
	if err != nil || rst != nil {
		t.Fatalf("duplicated reopen SYN: err=%v, RST sent=%v", err, rst != nil)
	}
}
