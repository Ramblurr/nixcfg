// Package secrets resolves policy.SecretRef values: literal: refs directly,
// op:// refs through a 1Password Connect server. Values are cached for a
// short TTL. Once an entry expires, a failed refresh is an error: a value
// revoked in 1Password is never served stale.
package secrets

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	"github.com/ramblurr/nixcfg/pkgs/ts-egress/internal/policy"
)

// Fetcher reads one op:// reference from 1Password.
type Fetcher interface {
	Fetch(ctx context.Context, ref policy.OPRef) (string, error)
}

// Store resolves secret references. The zero value resolves only literals.
type Store struct {
	// OP fetches op:// references. Nil fails every one.
	OP Fetcher
	// TTL is how long a fetched value is served. Zero means one minute.
	TTL time.Duration
	// FetchTimeout bounds one fetch, detached from the request. Zero means
	// ten seconds.
	FetchTimeout time.Duration

	now   func() time.Time
	mu    sync.Mutex // guards cache and every entry's value and expiry
	cache map[policy.OPRef]*entry
}

type entry struct {
	mu      sync.Mutex // held during a fetch, so callers share it
	value   string
	expires time.Time
}

// ErrNotConfigured means an op:// reference reached a Store without
// 1Password.
var ErrNotConfigured = errors.New("1password connect is not configured")

// Get returns the value ref points at. Errors never contain the value.
func (s *Store) Get(ctx context.Context, ref policy.SecretRef) (string, error) {
	switch ref.Scheme {
	case policy.SecretLiteral:
		return ref.Ref, nil
	case policy.SecretOnePassword:
		return s.op(ctx, ref)
	}
	return "", fmt.Errorf("unknown secret scheme %q", ref.Scheme)
}

func (s *Store) op(ctx context.Context, ref policy.SecretRef) (string, error) {
	if s.OP == nil {
		return "", ErrNotConfigured
	}
	e := s.entry(ref.OP)
	e.mu.Lock()
	defer e.mu.Unlock()
	now := s.clock()
	s.mu.Lock()
	v, fresh := e.value, now.Before(e.expires)
	if !fresh {
		e.value, e.expires = "", time.Time{}
	}
	s.mu.Unlock()
	if fresh {
		return v, nil
	}
	timeout := s.FetchTimeout
	if timeout == 0 {
		timeout = 10 * time.Second
	}
	fctx, cancel := context.WithTimeout(context.WithoutCancel(ctx), timeout)
	defer cancel()
	v, err := s.OP.Fetch(fctx, ref.OP)
	if err != nil {
		return "", fmt.Errorf("%s: %w", ref.Ref, err)
	}
	if v == "" {
		return "", fmt.Errorf("%s: empty value", ref.Ref)
	}
	if !policy.SentVerbatim(v) {
		return "", fmt.Errorf("%s: value has control bytes or surrounding whitespace", ref.Ref)
	}
	ttl := s.TTL
	if ttl == 0 {
		ttl = time.Minute
	}
	s.mu.Lock()
	e.value, e.expires = v, now.Add(ttl)
	s.mu.Unlock()
	return v, nil
}

func (s *Store) entry(key policy.OPRef) *entry {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.cache == nil {
		s.cache = map[policy.OPRef]*entry{}
	}
	e, ok := s.cache[key]
	if !ok {
		e = &entry{}
		s.cache[key] = e
	}
	return e
}

func (s *Store) clock() time.Time {
	if s.now != nil {
		return s.now()
	}
	return time.Now()
}
