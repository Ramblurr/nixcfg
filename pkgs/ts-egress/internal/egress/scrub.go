package egress

import (
	"cmp"
	"maps"
	"net/http"
	"slices"
	"strings"
)

// scrubber maps each secret value one request sent upstream to what the
// tenant sent in its place: the placeholder, or for a Basic credential
// built from one, the tenant's base64. It rewrites response header values
// only, matching exact bytes, so it is not leak prevention.
type scrubber map[string]string

// replacer replaces the longest value first where two start together.
func (s scrubber) replacer() *strings.Replacer {
	reals := slices.SortedFunc(maps.Keys(s), func(a, b string) int { return cmp.Compare(len(b), len(a)) })
	pairs := make([]string, 0, 2*len(reals))
	for _, r := range reals {
		pairs = append(pairs, r, s[r])
	}
	return strings.NewReplacer(pairs...)
}

// inName reports whether a header name holds a value in any letter case,
// as canonicalization changes only case. A name cannot be rewritten.
func (s scrubber) inName(name string) bool {
	name = strings.ToLower(name)
	for r := range s {
		if strings.Contains(name, strings.ToLower(r)) {
			return true
		}
	}
	return false
}

func (s scrubber) header(h http.Header) {
	if len(s) == 0 {
		return
	}
	r := s.replacer()
	for _, vs := range h {
		for i, v := range vs {
			vs[i] = r.Replace(v)
		}
	}
}

func (s scrubber) string(v string) string {
	if len(s) == 0 {
		return v
	}
	return s.replacer().Replace(v)
}
