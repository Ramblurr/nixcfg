package main

// Local SOCKS5 and HTTP proxies that dial through this process's tsnet
// server. Adapted from cmd/ts-unplug-proxy so one ts-plug node can both
// expose an upstream and give local clients outbound tailnet access.

import (
	"context"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"math/rand/v2"
	"net"
	"net/http"
	"net/http/httputil"
	"net/netip"
	"slices"
	"strings"
	"sync"
	"time"

	"golang.org/x/net/dns/dnsmessage"
	"tailscale.com/client/local"
	"tailscale.com/ipn/ipnstate"
	"tailscale.com/net/socks5"
	"tailscale.com/tsnet"
)

type tailnetDialer struct {
	ts *tsnet.Server
	lc *local.Client
	// viaNetstack, when set, carries every dial, so an address outside the
	// tailnet goes through the exit node or fails. It is set with an exit
	// node, because tsnet's Dial falls back to the host network while the
	// exit node does not resolve to a peer.
	viaNetstack netstackDial
	// dnsResolver answers names outside the tailnet when viaNetstack is
	// set. Queries to it go over the netstack like any other traffic, so
	// they reach the exit node as port 53 flows, not as LocalAPI lookups
	// that the exit node's PeerAPI resolver would answer.
	dnsResolver netip.AddrPort
}

func (d *tailnetDialer) Dial(ctx context.Context, network, addr string) (net.Conn, error) {
	resolvedAddr, resolved, err := d.resolveAddr(ctx, network, addr)
	if err != nil {
		return nil, err
	}
	if d.viaNetstack != nil {
		if !resolved {
			return nil, fmt.Errorf("resolve %q through the tailnet: no addresses", addr)
		}
		dst, err := netip.ParseAddrPort(resolvedAddr)
		if err != nil {
			return nil, err
		}
		return d.viaNetstack(ctx, network, dst)
	}
	if !resolved {
		slog.Debug("dialing through tailnet with original host",
			slog.String("network", network),
			slog.String("addr", addr),
		)
		return d.ts.Dial(ctx, network, addr)
	}
	slog.Debug("dialing through tailnet",
		slog.String("network", network),
		slog.String("addr", addr),
		slog.String("resolved_addr", resolvedAddr),
	)
	return d.ts.Dial(ctx, network, resolvedAddr)
}

func (d *tailnetDialer) resolveAddr(ctx context.Context, network, addr string) (resolvedAddr string, resolved bool, err error) {
	host, port, err := net.SplitHostPort(addr)
	if err != nil {
		return "", false, err
	}
	if ip, err := netip.ParseAddr(host); err == nil {
		return net.JoinHostPort(ip.String(), port), true, nil
	}

	status, err := d.lc.Status(ctx)
	if err != nil {
		slog.Debug("tailnet status lookup failed", slog.String("host", host), slog.Any("error", err))
	}
	ips := lookupMagicDNS(status, network, host)
	// Through an exit node, a tailnet name missing from the netmap stays
	// unresolved rather than leaking to the internet resolver.
	if len(ips) == 0 && (d.viaNetstack == nil || !isStrictTailnetHost(status, host)) {
		ips, err = d.lookupIP(ctx, network, host)
	}
	if err != nil {
		// Through an exit node there is nothing to fall back to, and the
		// resolver's answer says why the name is unreachable.
		if d.viaNetstack != nil || isStrictTailnetHost(status, host) {
			return "", false, fmt.Errorf("tailnet DNS lookup for %q: %w", host, err)
		}
		slog.Debug("tailnet DNS lookup failed; falling back to normal tsnet dial",
			slog.String("host", host),
			slog.Any("error", err),
		)
		return "", false, nil
	}
	if len(ips) == 0 {
		if isStrictTailnetHost(status, host) {
			return "", false, fmt.Errorf("tailnet DNS lookup for %q returned no addresses", host)
		}
		return "", false, nil
	}

	resolvedAddr = net.JoinHostPort(ips[0].String(), port)
	slog.Debug("tailnet DNS resolved",
		slog.String("host", host),
		slog.String("network", network),
		slog.String("resolved_addr", resolvedAddr),
	)
	return resolvedAddr, true, nil
}

func lookupMagicDNS(status *ipnstate.Status, network, host string) []netip.Addr {
	if status == nil {
		return nil
	}
	hostKey := dnsMapKey(host)
	var ips []netip.Addr
	addPeerIPs := func(peer *ipnstate.PeerStatus) {
		if peer == nil || !peerMatchesHost(status, peer, hostKey) {
			return
		}
		for _, ip := range peer.TailscaleIPs {
			if ipMatchesNetwork(network, ip) {
				ips = append(ips, ip)
			}
		}
	}

	addPeerIPs(status.Self)
	for _, peer := range status.Peer {
		addPeerIPs(peer)
	}
	if len(ips) == 0 {
		return nil
	}

	slog.Debug("tailnet status resolved",
		slog.String("host", host),
		slog.String("network", network),
		slog.String("ips", fmt.Sprint(ips)),
	)
	return ips
}

func peerMatchesHost(status *ipnstate.Status, peer *ipnstate.PeerStatus, hostKey string) bool {
	for _, name := range peerDNSNames(status, peer) {
		if dnsMapKey(name) == hostKey {
			return true
		}
	}
	return false
}

func peerDNSNames(status *ipnstate.Status, peer *ipnstate.PeerStatus) []string {
	var names []string
	if peer.DNSName != "" {
		names = append(names, peer.DNSName)
	}
	if peer.HostName != "" {
		names = append(names, peer.HostName)
	}

	for _, suffix := range magicDNSSuffixes(status) {
		for _, name := range slices.Clone(names) {
			name = strings.TrimSuffix(name, ".")
			if name == "" {
				continue
			}
			if strings.HasSuffix(dnsMapKey(name), "."+dnsMapKey(suffix)) {
				short := strings.TrimSuffix(name, "."+strings.TrimSuffix(suffix, "."))
				names = append(names, short)
			} else if peer.HostName != "" {
				names = append(names, strings.TrimSuffix(peer.HostName, ".")+"."+strings.TrimSuffix(suffix, "."))
			}
		}
	}

	return names
}

func magicDNSSuffixes(status *ipnstate.Status) []string {
	if status == nil {
		return nil
	}
	var suffixes []string
	if status.MagicDNSSuffix != "" {
		suffixes = append(suffixes, status.MagicDNSSuffix)
	}
	if status.CurrentTailnet != nil && status.CurrentTailnet.MagicDNSSuffix != "" {
		suffixes = append(suffixes, status.CurrentTailnet.MagicDNSSuffix)
	}
	return suffixes
}

func isStrictTailnetHost(status *ipnstate.Status, host string) bool {
	hostKey := dnsMapKey(host)
	if !strings.Contains(hostKey, ".") {
		return true
	}
	for _, suffix := range magicDNSSuffixes(status) {
		suffixKey := dnsMapKey(suffix)
		if hostKey == suffixKey || strings.HasSuffix(hostKey, "."+suffixKey) {
			return true
		}
	}
	return strings.HasSuffix(hostKey, ".ts.net")
}

func dnsMapKey(name string) string {
	return strings.ToLower(strings.TrimSuffix(name, "."))
}

func ipMatchesNetwork(network string, ip netip.Addr) bool {
	switch {
	case strings.HasSuffix(network, "4"):
		return ip.Is4()
	case strings.HasSuffix(network, "6"):
		return ip.Is6()
	default:
		return true
	}
}

func (d *tailnetDialer) lookupIP(ctx context.Context, network, host string) ([]netip.Addr, error) {
	query := d.queryLocalAPI
	if d.viaNetstack != nil {
		query = d.queryResolver
	}
	var ips []netip.Addr
	var errs []error
	for _, queryType := range queryTypesForNetwork(network) {
		response, err := query(ctx, host, queryType)
		if err != nil {
			errs = append(errs, fmt.Errorf("%s query: %w", queryType, err))
			continue
		}
		queryIPs, err := parseDNSIPs(response, queryType)
		if err != nil {
			errs = append(errs, fmt.Errorf("%s response: %w", queryType, err))
			continue
		}
		slog.Debug("DNS query complete",
			slog.String("host", host),
			slog.String("type", queryType),
			slog.Int("answers", len(queryIPs)),
		)
		ips = append(ips, queryIPs...)
	}
	if len(ips) > 0 {
		return ips, nil
	}
	return nil, errors.Join(errs...)
}

func (d *tailnetDialer) queryLocalAPI(ctx context.Context, host, queryType string) ([]byte, error) {
	response, resolvers, err := d.lc.QueryDNS(ctx, dnsQueryName(host), queryType)
	if err == nil {
		slog.Debug("tailnet DNS resolvers", slog.String("host", host), slog.String("resolvers", fmt.Sprint(resolvers)))
	}
	return response, err
}

// dnsQueryTimeout bounds one query to dnsResolver.
const dnsQueryTimeout = 5 * time.Second

// queryResolver sends one query for host over the netstack to dnsResolver
// and returns the response that answers it.
func (d *tailnetDialer) queryResolver(ctx context.Context, host, queryType string) ([]byte, error) {
	name, err := dnsmessage.NewName(dnsQueryName(host))
	if err != nil {
		return nil, err
	}
	qtype := dnsmessage.TypeA
	if queryType == "AAAA" {
		qtype = dnsmessage.TypeAAAA
	}
	id := uint16(rand.Uint32())
	// EDNS lets the resolver say why it refuses a name (RFC 8914).
	var opt dnsmessage.ResourceHeader
	if err := opt.SetEDNS0(1232, dnsmessage.RCodeSuccess, false); err != nil {
		return nil, err
	}
	query, err := (&dnsmessage.Message{
		Header:      dnsmessage.Header{ID: id, RecursionDesired: true},
		Questions:   []dnsmessage.Question{{Name: name, Type: qtype, Class: dnsmessage.ClassINET}},
		Additionals: []dnsmessage.Resource{{Header: opt, Body: &dnsmessage.OPTResource{}}},
	}).Pack()
	if err != nil {
		return nil, err
	}
	ctx, cancel := context.WithTimeout(ctx, dnsQueryTimeout)
	defer cancel()
	conn, err := d.viaNetstack(ctx, "udp", d.dnsResolver)
	if err != nil {
		return nil, err
	}
	defer conn.Close()
	deadline, _ := ctx.Deadline()
	_ = conn.SetDeadline(deadline)
	if _, err := conn.Write(query); err != nil {
		return nil, err
	}
	buf := make([]byte, 65535)
	n, err := conn.Read(buf)
	if err != nil {
		return nil, err
	}
	var p dnsmessage.Parser
	h, err := p.Start(buf[:n])
	if err != nil {
		return nil, err
	}
	if !h.Response || h.ID != id {
		return nil, errors.New("DNS response does not match the query")
	}
	return buf[:n], nil
}

// parseDNSResolver checks -dns-resolver against -exit-node: an exit node
// needs a numeric resolver address, and the resolver means nothing
// without one.
func parseDNSResolver(exitNode, resolver string) (netip.AddrPort, error) {
	switch {
	case exitNode == "" && resolver == "":
		return netip.AddrPort{}, nil
	case exitNode == "":
		return netip.AddrPort{}, errors.New("-dns-resolver needs -exit-node")
	case resolver == "":
		return netip.AddrPort{}, errors.New("-exit-node needs -dns-resolver")
	}
	ap, err := netip.ParseAddrPort(resolver)
	if err != nil || ap.Port() == 0 {
		return netip.AddrPort{}, fmt.Errorf("-dns-resolver %q: want a numeric ip:port", resolver)
	}
	return ap, nil
}

func dnsQueryName(host string) string {
	if strings.HasSuffix(host, ".") {
		return host
	}
	return host + "."
}

func queryTypesForNetwork(network string) []string {
	if strings.HasSuffix(network, "4") {
		return []string{"A"}
	}
	if strings.HasSuffix(network, "6") {
		return []string{"AAAA"}
	}
	return []string{"A", "AAAA"}
}

func parseDNSIPs(response []byte, queryType string) ([]netip.Addr, error) {
	var parser dnsmessage.Parser
	header, err := parser.Start(response)
	if err != nil {
		return nil, err
	}
	if header.RCode != dnsmessage.RCodeSuccess {
		return nil, &dnsError{rcode: header.RCode, infoCode: extendedError(response)}
	}
	if err := parser.SkipAllQuestions(); err != nil {
		return nil, err
	}

	var ips []netip.Addr
	for {
		answerHeader, err := parser.AnswerHeader()
		if errors.Is(err, dnsmessage.ErrSectionDone) {
			return ips, nil
		}
		if err != nil {
			return nil, err
		}

		switch {
		case queryType == "A" && answerHeader.Type == dnsmessage.TypeA:
			answer, err := parser.AResource()
			if err != nil {
				return nil, err
			}
			ips = append(ips, netip.AddrFrom4(answer.A))
		case queryType == "AAAA" && answerHeader.Type == dnsmessage.TypeAAAA:
			answer, err := parser.AAAAResource()
			if err != nil {
				return nil, err
			}
			ips = append(ips, netip.AddrFrom16(answer.AAAA))
		default:
			if err := parser.SkipAnswer(); err != nil {
				return nil, err
			}
		}
	}
}

// dnsError is a resolver answer other than success, with the Extended DNS
// Error (RFC 8914) it carried.
type dnsError struct {
	rcode    dnsmessage.RCode
	infoCode int // -1 when the answer carried none
}

func (e *dnsError) Error() string {
	msg := "DNS answer " + rcodeName(e.rcode)
	switch e.infoCode {
	case -1:
		return msg
	case edeBlocked:
		return msg + " (Extended DNS Error 15: Blocked)"
	case edeProhibited:
		return msg + " (Extended DNS Error 18: Prohibited)"
	default:
		return fmt.Sprintf("%s (Extended DNS Error %d)", msg, e.infoCode)
	}
}

// Extended DNS Error codes that mean the resolver refused the name by
// policy: it is on a blocklist, or the client may not ask at all.
const (
	edeBlocked    = 15
	edeProhibited = 18
	optionEDE     = 15 // EDNS option code of an Extended DNS Error
)

// blocked reports whether the resolver refused the name by policy.
func (e *dnsError) blocked() bool {
	return e.rcode == dnsmessage.RCodeRefused && (e.infoCode == edeBlocked || e.infoCode == edeProhibited)
}

// rcodeName spells rcode as RFC 8499 does, the form Proxy-Status uses.
func rcodeName(rcode dnsmessage.RCode) string {
	switch rcode {
	case dnsmessage.RCodeFormatError:
		return "FORMERR"
	case dnsmessage.RCodeServerFailure:
		return "SERVFAIL"
	case dnsmessage.RCodeNameError:
		return "NXDOMAIN"
	case dnsmessage.RCodeNotImplemented:
		return "NOTIMP"
	case dnsmessage.RCodeRefused:
		return "REFUSED"
	default:
		return fmt.Sprintf("RCODE%d", rcode)
	}
}

// extendedError returns the first Extended DNS Error code in response, or
// -1 when it has none.
func extendedError(response []byte) int {
	var m dnsmessage.Message
	if m.Unpack(response) != nil {
		return -1
	}
	for _, r := range m.Additionals {
		opt, ok := r.Body.(*dnsmessage.OPTResource)
		if !ok {
			continue
		}
		for _, o := range opt.Options {
			if o.Code == optionEDE && len(o.Data) >= 2 {
				return int(o.Data[0])<<8 | int(o.Data[1])
			}
		}
	}
	return -1
}

// proxyFailure maps a dial error to the status the proxy answers and its
// Proxy-Status header (RFC 9209), empty when there is nothing to add. A
// name the resolver refused by policy is 403, as RFC 9209 recommends for
// a denied request; every other failure is 502.
func proxyFailure(err error) (status int, proxyStatus string) {
	var de *dnsError
	if !errors.As(err, &de) {
		return http.StatusBadGateway, ""
	}
	proxyStatus = fmt.Sprintf("ts-plug; error=dns_error; rcode=%q", rcodeName(de.rcode))
	if de.infoCode >= 0 {
		proxyStatus += fmt.Sprintf("; info-code=%d", de.infoCode)
	}
	if de.blocked() {
		return http.StatusForbidden, proxyStatus
	}
	return http.StatusBadGateway, proxyStatus
}

// writeProxyFailure answers a request whose dial failed with err.
func writeProxyFailure(w http.ResponseWriter, err error) {
	status, proxyStatus := proxyFailure(err)
	if proxyStatus != "" {
		w.Header().Set("Proxy-Status", proxyStatus)
	}
	http.Error(w, err.Error(), status)
}

func startSOCKS5Proxy(listener net.Listener, dialer func(context.Context, string, string) (net.Conn, error), fail func(error)) {
	proxy := &socks5.Server{
		Logf: func(format string, args ...any) {
			slog.Debug(fmt.Sprintf(format, args...))
		},
		Dialer: dialer,
	}
	go func() {
		fail(fmt.Errorf("SOCKS5 proxy stopped: %w", proxy.Serve(listener)))
	}()
}

func startHTTPProxy(listener net.Listener, dialer func(context.Context, string, string) (net.Conn, error), fail func(error)) {
	server := &http.Server{
		Handler: httpProxyHandler(dialer),
	}
	go func() {
		fail(fmt.Errorf("HTTP proxy stopped: %w", server.Serve(listener)))
	}()
}

func httpProxyHandler(dialer func(context.Context, string, string) (net.Conn, error)) http.Handler {
	proxy := &httputil.ReverseProxy{
		Director: func(r *http.Request) {},
		Transport: &http.Transport{
			DialContext: dialer,
		},
		ErrorHandler: func(w http.ResponseWriter, _ *http.Request, err error) {
			writeProxyFailure(w, err)
		},
	}

	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		slog.Debug("HTTP proxy request", slog.String("method", r.Method), slog.String("target", r.RequestURI))

		if r.Method != http.MethodConnect {
			if strings.HasPrefix(r.RequestURI, "/") || r.RequestURI == "*" {
				http.Error(w, "bogus RequestURI; must be absolute URL or CONNECT", http.StatusBadRequest)
				return
			}
			proxy.ServeHTTP(w, r)
			return
		}

		dst := r.RequestURI
		backend, err := dialer(r.Context(), "tcp", dst)
		if err != nil {
			w.Header().Set("Tailscale-Connect-Error", err.Error())
			writeProxyFailure(w, err)
			return
		}
		defer backend.Close()

		client, clientBuf, err := http.NewResponseController(w).Hijack()
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		defer client.Close()

		if _, err := io.WriteString(client, "HTTP/1.1 200 OK\r\n\r\n"); err != nil {
			return
		}

		var clientSrc io.Reader = clientBuf
		if clientBuf.Reader.Buffered() == 0 {
			clientSrc = client
		}

		splice(client, clientSrc, backend)
	})
}

// splice copies both ways between client and backend until both directions
// end. clientSrc is what the client sent, which differs from client when
// some of it is already buffered.
func splice(client net.Conn, clientSrc io.Reader, backend net.Conn) {
	closeBoth := func() {
		client.Close()
		backend.Close()
	}
	var wg sync.WaitGroup
	wg.Go(func() { relay(client, backend, closeBoth) })
	wg.Go(func() { relay(backend, clientSrc, closeBoth) })
	wg.Wait()
}

// relay copies src to dst. On EOF it half-closes dst so the peer sees the
// end of the stream while the reverse copy finishes. On an error, or when
// dst cannot half-close, it calls closeBoth, which also ends the reverse
// copy.
func relay(dst net.Conn, src io.Reader, closeBoth func()) {
	if _, err := io.Copy(dst, src); err != nil {
		closeBoth()
		return
	}
	if cw, ok := dst.(interface{ CloseWrite() error }); ok {
		cw.CloseWrite()
		return
	}
	closeBoth()
}

// startLocalProxies starts the requested local proxies. A proxy that stops
// serving reports its error to fail.
func startLocalProxies(dialer func(context.Context, string, string) (net.Conn, error), socks5Addr, httpAddr string, fail func(error)) error {
	if socks5Addr != "" {
		listener, err := net.Listen("tcp", socks5Addr)
		if err != nil {
			return fmt.Errorf("listen for SOCKS5 on %s: %w", socks5Addr, err)
		}
		startSOCKS5Proxy(listener, dialer, fail)
		slog.Info("SOCKS5 proxy listening", slog.String("local", listener.Addr().String()))
	}
	if httpAddr != "" {
		listener, err := net.Listen("tcp", httpAddr)
		if err != nil {
			return fmt.Errorf("listen for HTTP proxy on %s: %w", httpAddr, err)
		}
		startHTTPProxy(listener, dialer, fail)
		slog.Info("HTTP proxy listening", slog.String("local", listener.Addr().String()))
	}
	return nil
}
