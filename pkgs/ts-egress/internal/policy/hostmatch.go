// Copyright 2025 The iron-proxy Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// Adapted from github.com/ironsh/iron-proxy internal/hostmatch/hostmatch.go
// at commit c32e47d653256ad7b1638214f60b5859ff94b268. Changes: a HostPattern
// type validated at parse time, and ParseName and ParseHost for flow names.

package policy

import (
	"fmt"
	"net/netip"
	"path"
	"strings"
)

// HostPattern is a validated, lower-case domain glob. "*" matches any host,
// "*.example.com" matches every subdomain and example.com itself, and other
// patterns use path.Match semantics.
type HostPattern string

// ParseHostPattern validates and lower-cases a host glob: labels as a name
// spells them, in which the glob characters *, ?, [, ] and ^ may also appear.
func ParseHostPattern(s string) (HostPattern, error) {
	s = asciiLower(s)
	if !labelsOK(s, func(c byte) bool { return ldh(c) || strings.IndexByte("*?[]^", c) >= 0 }) {
		return "", fmt.Errorf("invalid host pattern %q", s)
	}
	if _, err := path.Match(s, ""); err != nil {
		return "", fmt.Errorf("invalid host pattern %q: %w", s, err)
	}
	return HostPattern(s), nil
}

// Matches reports whether name matches the pattern. A name ParseName or
// ParseHost did not return never matches.
func (h HostPattern) Matches(name string) bool {
	if !isName(name) && !isIPv6(name) {
		return false
	}
	pattern := string(h)
	if pattern == "*" {
		return true
	}
	if suffix, ok := strings.CutPrefix(pattern, "*."); ok {
		return strings.HasSuffix(name, "."+suffix) || name == suffix
	}
	matched, _ := path.Match(pattern, name)
	return matched
}

func matchAny(patterns []HostPattern, name string) bool {
	for _, p := range patterns {
		if p.Matches(name) {
			return true
		}
	}
	return false
}

// NormalizeName lower-cases a DNS name, SNI or Host and drops a trailing dot.
func NormalizeName(name string) string {
	return strings.TrimSuffix(asciiLower(name), ".")
}

// ParseName returns a DNS name or SNI in canonical form: lower case, one
// trailing dot dropped. It reports false unless what remains is labels of
// letters, digits and hyphens, each with an optional leading underscore,
// joined by single dots. Every other byte is refused rather than given a
// meaning: a resolver, server or certificate could read it as a name no
// pattern spells.
func ParseName(raw string) (string, bool) {
	n := NormalizeName(raw)
	return n, isName(n)
}

// ParseHost returns the name in an HTTP Host value: an optional numeric
// port dropped, then a name as ParseName reads it, or an IPv6 literal in
// brackets, returned bare in canonical form.
func ParseHost(raw string) (string, bool) {
	host, port := raw, ""
	rest, bracketed := strings.CutPrefix(raw, "[")
	if bracketed {
		end := strings.IndexByte(rest, ']')
		if end < 0 {
			return "", false
		}
		host, port = rest[:end], rest[end+1:]
	} else if i := strings.IndexByte(raw, ':'); i >= 0 {
		host, port = raw[:i], raw[i:]
	}
	if port != "" {
		digits, ok := strings.CutPrefix(port, ":")
		if !ok || digits == "" || len(digits) > 5 || strings.Trim(digits, "0123456789") != "" {
			return "", false
		}
	}
	if bracketed {
		ip, err := netip.ParseAddr(host)
		if err != nil || !ip.Is6() || ip.Zone() != "" {
			return "", false
		}
		return ip.String(), true
	}
	return ParseName(host)
}

// isName reports whether n is lower-case labels as ParseName accepts them.
func isName(n string) bool {
	return labelsOK(n, func(c byte) bool { return ldh(c) })
}

// isIPv6 reports whether n is an IPv6 literal as ParseHost returns it.
func isIPv6(n string) bool {
	ip, err := netip.ParseAddr(n)
	return err == nil && ip.Is6() && ip.Zone() == "" && ip.String() == n
}

// labelsOK reports whether s is non-empty labels joined by single dots,
// each an optional leading underscore and then bytes ok accepts.
func labelsOK(s string, ok func(byte) bool) bool {
	if s == "" {
		return false
	}
	for label := range strings.SplitSeq(s, ".") {
		label = strings.TrimPrefix(label, "_")
		if label == "" {
			return false
		}
		for i := 0; i < len(label); i++ {
			if !ok(label[i]) {
				return false
			}
		}
	}
	return true
}

func ldh(c byte) bool {
	return 'a' <= c && c <= 'z' || '0' <= c && c <= '9' || c == '-'
}

func asciiLower(s string) string {
	b := []byte(s)
	for i, c := range b {
		if 'A' <= c && c <= 'Z' {
			b[i] = c + ('a' - 'A')
		}
	}
	return string(b)
}
