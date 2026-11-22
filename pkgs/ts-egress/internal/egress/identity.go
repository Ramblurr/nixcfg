package egress

import (
	"context"
	"errors"
	"log/slog"
	"net/netip"
	"sync"
	"time"

	"tailscale.com/client/local"
	"tailscale.com/client/tailscale/apitype"
	"tailscale.com/ipn"
	"tailscale.com/tailcfg"

	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/policy"
)

// Caller is the tailnet identity behind a flow and its parsed policy. A
// Caller whose Reason is set must have every flow denied.
type Caller struct {
	Node   string
	Tags   []string
	Policy policy.Policy
	Reason policy.Reason
	nodeID tailcfg.NodeID
}

// WhoIser is the subset of tailscale.com/client/local.Client we use.
type WhoIser interface {
	WhoIs(ctx context.Context, remoteAddr string) (*apitype.WhoIsResponse, error)
}

// Identities resolves and caches Callers per source tailnet IP.
type Identities struct {
	who WhoIser
	ttl time.Duration
	now func() time.Time

	mu    sync.Mutex
	cache map[netip.Addr]cached
	// gen counts invalidations. A WhoIs result read across one may hold
	// grants from before it, so Lookup drops it.
	gen uint64
}

type cached struct {
	caller  *Caller
	expires time.Time
}

func NewIdentities(who WhoIser, ttl time.Duration) *Identities {
	return &Identities{who: who, ttl: ttl, now: time.Now, cache: map[netip.Addr]cached{}}
}

// lookupAttempts bounds WhoIs retries while netmap changes keep landing.
const lookupAttempts = 3

// Lookup returns the Caller for src. It never returns nil: a failed lookup
// yields a Caller with a deny Reason, cached like any other result so a
// tenant cannot hammer tailscaled. A result that an invalidation overtook
// is retried, and denied if every attempt is overtaken.
func (ids *Identities) Lookup(ctx context.Context, src netip.Addr) *Caller {
	src = src.Unmap()
	for range lookupAttempts {
		now := ids.now()
		ids.mu.Lock()
		c, ok := ids.cache[src]
		gen := ids.gen
		ids.mu.Unlock()
		if ok && now.Before(c.expires) {
			return c.caller
		}
		caller := ids.resolve(ctx, src)
		ids.mu.Lock()
		current := ids.gen == gen
		if current {
			ids.cache[src] = cached{caller: caller, expires: now.Add(ids.ttl)}
		}
		ids.mu.Unlock()
		if current {
			return caller
		}
	}
	return &Caller{Reason: policy.ReasonNoIdentity}
}

func (ids *Identities) resolve(ctx context.Context, src netip.Addr) *Caller {
	ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()
	res, err := ids.who.WhoIs(ctx, src.String())
	if err != nil || res == nil || res.Node == nil {
		slog.Debug("whois failed", "src", src, "err", err)
		return &Caller{Reason: policy.ReasonNoIdentity}
	}
	c := &Caller{Node: res.Node.ComputedName, Tags: res.Node.Tags, nodeID: res.Node.ID}
	if c.Node == "" {
		c.Node = res.Node.Name
	}
	p, err := policy.Parse(res.CapMap)
	switch {
	case errors.Is(err, policy.ErrNoCaps):
		c.Reason = policy.ReasonNoCaps
	case err != nil:
		slog.Warn("invalid egress policy", "node", c.Node, "tags", c.Tags, "err", err)
		c.Reason = policy.ReasonBadPolicy
	default:
		c.Policy = p
	}
	return c
}

// Invalidate drops cached entries affected by a netmap change. Packet filter
// changes are not announced on the bus, so the TTL still bounds staleness.
func (ids *Identities) Invalidate(n *ipn.Notify) {
	ids.mu.Lock()
	defer ids.mu.Unlock()
	ids.gen++
	if n.SelfChange != nil {
		clear(ids.cache)
		return
	}
	gone := map[tailcfg.NodeID]bool{}
	for _, id := range n.PeersRemoved {
		gone[id] = true
	}
	for _, node := range n.PeersChanged {
		gone[node.ID] = true
		for _, p := range node.Addresses {
			delete(ids.cache, p.Addr())
		}
	}
	for ip, c := range ids.cache {
		if gone[c.caller.nodeID] {
			delete(ids.cache, ip)
		}
	}
}

// Watch invalidates cache entries on netmap changes until ctx ends,
// reconnecting to tailscaled after errors.
func (ids *Identities) Watch(ctx context.Context, lc *local.Client) {
	for ctx.Err() == nil {
		w, err := lc.WatchIPNBus(ctx, ipn.NotifyPeerChanges)
		if err == nil {
			for {
				n, err := w.Next()
				if err != nil {
					break
				}
				ids.Invalidate(&n)
			}
			w.Close()
		}
		ids.mu.Lock()
		ids.gen++
		clear(ids.cache)
		ids.mu.Unlock()
		select {
		case <-ctx.Done():
		case <-time.After(2 * time.Second):
		}
	}
}
