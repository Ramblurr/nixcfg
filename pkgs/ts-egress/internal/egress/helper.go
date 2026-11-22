package egress

import (
	"log/slog"
	"net"
	"net/http"
	"net/netip"
	"slices"
	"strconv"
	"time"

	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/policy"
)

// DefaultHelperHosts are the Tailscale control, logging and DERP hosts a
// tenant's own tailscale node needs to log in and keep its tunnels up.
var DefaultHelperHosts = []string{
	"controlplane.tailscale.com",
	"login.tailscale.com",
	"log.tailscale.com",
	"derp*.tailscale.com",
}

// Helper is an allow-only HTTP CONNECT proxy for Tailscale's own hosts. It
// has no caller identity: anything that can reach it may reach only Hosts
// on Ports, dialled by name and never to a floored or private address.
type Helper struct {
	Hosts []policy.HostPattern
	Ports []uint16
	Floor policy.Floor
	Audit *slog.Logger
}

func (h *Helper) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	rec := &record{start: time.Now(), proto: policy.TCP, src: addrPortString(r.RemoteAddr)}
	defer logRecord(h.Audit, rec)
	rec.verdict = policy.Deny{Reason: policy.ReasonNoRule}
	if r.Method != http.MethodConnect {
		http.Error(w, "CONNECT only", http.StatusMethodNotAllowed)
		return
	}
	host, portStr, err := net.SplitHostPort(r.Host)
	port, perr := strconv.ParseUint(portStr, 10, 16)
	host = policy.NormalizeName(host)
	rec.host = host
	if err != nil || perr != nil || !slices.Contains(h.Ports, uint16(port)) || !h.allowed(host) {
		http.Error(w, "destination not allowed", http.StatusForbidden)
		return
	}
	rec.verdict = policy.Relay{Rule: "helper"}
	floorOnly := &policy.Policy{Private: policy.Private{Block: true}}
	d := net.Dialer{Timeout: 10 * time.Second, Control: h.Floor.DialControl(floorOnly)}
	up, err := d.DialContext(r.Context(), "tcp", net.JoinHostPort(host, portStr))
	if err != nil {
		rec.fail(err)
		http.Error(w, "dial failed", http.StatusBadGateway)
		return
	}
	defer up.Close()
	rec.dst = addrPort(up.RemoteAddr())
	hj, ok := w.(http.Hijacker)
	if !ok {
		rec.err = http.ErrNotSupported
		return
	}
	client, buf, err := hj.Hijack()
	if err != nil {
		rec.err = err
		return
	}
	defer client.Close()
	if _, err := client.Write([]byte("HTTP/1.1 200 Connection established\r\n\r\n")); err != nil {
		rec.err = err
		return
	}
	if n := buf.Reader.Buffered(); n > 0 {
		pending, _ := buf.Reader.Peek(n)
		if _, err := up.Write(pending); err != nil {
			rec.err = err
			return
		}
		rec.up += int64(n)
	}
	sent, recv := splice(client, up)
	rec.up += sent
	rec.down = recv
}

func (h *Helper) allowed(host string) bool {
	for _, p := range h.Hosts {
		if p.Matches(host) {
			return true
		}
	}
	return false
}

func addrPortString(s string) netip.AddrPort {
	ap, err := netip.ParseAddrPort(s)
	if err != nil {
		return netip.AddrPort{}
	}
	return netip.AddrPortFrom(ap.Addr().Unmap(), ap.Port())
}
