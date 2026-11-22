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
// Adapted from github.com/ironsh/iron-proxy internal/dnsguard/dnsguard.go
// at commit c32e47d653256ad7b1638214f60b5859ff94b268. Changes: the check is
// the egress Floor plus the caller's Policy instead of a fixed CIDR list.

package policy

import (
	"fmt"
	"net"
	"net/netip"
	"strings"
	"syscall"
)

// DenyError is returned by DialControl when the address about to be
// connected is denied.
type DenyError struct {
	Addr   string
	Reason Reason
}

func (e *DenyError) Error() string { return fmt.Sprintf("dial %s denied: %s", e.Addr, e.Reason) }

// DialControl returns a net.Dialer.Control hook that re-checks every address
// Go is about to connect to, after DNS resolution. This catches a name that
// resolves into a denied range.
func (f Floor) DialControl(p *Policy) func(network, address string, c syscall.RawConn) error {
	return func(_, address string, _ syscall.RawConn) error {
		host, _, err := net.SplitHostPort(address)
		if err != nil {
			host = address
		}
		ipStr, _, _ := strings.Cut(host, "%")
		ip, err := netip.ParseAddr(ipStr)
		if err != nil {
			return &DenyError{Addr: address, Reason: ReasonFloor}
		}
		if r, denied := f.CheckAddr(p, ip); denied {
			return &DenyError{Addr: address, Reason: r}
		}
		return nil
	}
}
