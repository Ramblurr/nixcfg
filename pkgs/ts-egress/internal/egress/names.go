package egress

import (
	"net/netip"
	"sync"
	"time"
)

// Names remembers which names each caller resolved to which addresses
// through this egress, so raw TCP and UDP flows can match host rules.
type Names struct {
	now func() time.Time

	mu sync.Mutex
	m  map[nameKey]map[string]time.Time
}

type nameKey struct{ caller, dst netip.Addr }

const (
	minNameTTL = time.Minute
	maxNameTTL = time.Hour
)

func NewNames() *Names {
	return &Names{now: time.Now, m: map[nameKey]map[string]time.Time{}}
}

// Learn records that caller resolved name to dst, for ttl clamped to
// [minNameTTL, maxNameTTL].
func (n *Names) Learn(caller, dst netip.Addr, name string, ttl time.Duration) {
	ttl = min(max(ttl, minNameTTL), maxNameTTL)
	k := nameKey{caller.Unmap(), dst.Unmap()}
	n.mu.Lock()
	defer n.mu.Unlock()
	names := n.m[k]
	if names == nil {
		names = map[string]time.Time{}
		n.m[k] = names
	}
	names[name] = n.now().Add(ttl)
}

// For returns the unexpired names caller resolved to dst.
func (n *Names) For(caller, dst netip.Addr) []string {
	k := nameKey{caller.Unmap(), dst.Unmap()}
	now := n.now()
	n.mu.Lock()
	defer n.mu.Unlock()
	var out []string
	for name, exp := range n.m[k] {
		if now.Before(exp) {
			out = append(out, name)
		} else {
			delete(n.m[k], name)
		}
	}
	if len(n.m[k]) == 0 {
		delete(n.m, k)
	}
	return out
}

// Sweep drops expired entries. Run it periodically.
func (n *Names) Sweep() {
	now := n.now()
	n.mu.Lock()
	defer n.mu.Unlock()
	for k, names := range n.m {
		for name, exp := range names {
			if !now.Before(exp) {
				delete(names, name)
			}
		}
		if len(names) == 0 {
			delete(n.m, k)
		}
	}
}
