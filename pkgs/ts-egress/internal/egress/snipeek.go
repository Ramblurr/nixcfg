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
// Adapted from github.com/ironsh/iron-proxy internal/proxy/snipeek.go at
// commit c32e47d653256ad7b1638214f60b5859ff94b268. Changes: peek sniffs the
// first byte and reads either a TLS ClientHello for SNI or an HTTP/1 request
// head for Host, replaying every byte it consumed.

package egress

import (
	"bufio"
	"bytes"
	"context"
	"crypto/tls"
	"errors"
	"io"
	"net"
	"net/http"
	"time"
)

// maxPeekBytes caps how many bytes peek buffers from a client, so a slow or
// malicious client cannot grow the buffer without bound.
const maxPeekBytes = 16 * 1024

var errSNIPeekDone = errors.New("sni peek complete")

// peeked is what peek learned from the start of a client stream.
type peeked struct {
	SNI   string
	Host  string
	Bytes []byte
	// Incomplete is set when the stream starts like TLS or HTTP but ends,
	// stalls or overflows before the ClientHello or request head does.
	// Such a flow must be denied: its name could not be checked.
	Incomplete bool
}

// peek reads the start of conn until it can name the destination: the SNI
// of a TLS ClientHello, or the Host of an HTTP/1 request. Bytes holds
// everything consumed, for replay upstream. A client that sends nothing
// before the timeout, or a protocol that is neither, yields no name.
func peek(conn net.Conn, timeout time.Duration) peeked {
	deadline := time.Now().Add(timeout)
	_ = conn.SetReadDeadline(deadline)
	defer func() { _ = conn.SetReadDeadline(time.Time{}) }()

	rec := &recordingConn{Conn: conn}
	var first [1]byte
	if n, _ := io.ReadFull(rec, first[:]); n == 0 {
		return peeked{}
	}
	replay := &replayConn{recordingConn: rec, r: io.MultiReader(bytes.NewReader(first[:]), readerFunc(rec.Read))}
	var out peeked
	switch {
	case first[0] == 0x16:
		var ok bool
		out.SNI, ok = peekSNI(replay, deadline)
		out.Incomplete = !ok
	case 'A' <= first[0] && first[0] <= 'Z':
		var ok bool
		out.Host, ok = peekHost(replay)
		out.Incomplete = !ok && looksLikeHTTP(rec.buf.Bytes())
	}
	out.Bytes = rec.buf.Bytes()
	return out
}

// peekSNI reports the ClientHello's server name, and whether a whole
// ClientHello arrived.
func peekSNI(conn net.Conn, deadline time.Time) (string, bool) {
	var captured string
	var ok bool
	cfg := &tls.Config{
		GetConfigForClient: func(hello *tls.ClientHelloInfo) (*tls.Config, error) {
			captured, ok = hello.ServerName, true
			return nil, errSNIPeekDone
		},
	}
	ctx, cancel := context.WithDeadline(context.Background(), deadline)
	defer cancel()
	_ = tls.Server(conn, cfg).HandshakeContext(ctx)
	return captured, ok
}

// peekHost reports the request's Host as sent, and whether a request head
// parsed. Decide reads the name in it.
func peekHost(conn net.Conn) (string, bool) {
	req, err := http.ReadRequest(bufio.NewReader(conn))
	if err != nil {
		return "", false
	}
	return req.Host, true
}

// looksLikeHTTP reports whether b could be the start of an HTTP request
// line: upper-case letters, then a space or nothing yet. An SSH or SMTP
// banner breaks off at another byte.
func looksLikeHTTP(b []byte) bool {
	i := bytes.IndexFunc(b, func(r rune) bool { return r < 'A' || r > 'Z' })
	return i < 0 || b[i] == ' '
}

// recordingConn tees every byte read into buf and discards writes, so the
// TLS server never answers the client.
type recordingConn struct {
	net.Conn
	buf bytes.Buffer
}

func (c *recordingConn) Read(p []byte) (int, error) {
	if c.buf.Len() >= maxPeekBytes {
		return 0, io.ErrShortBuffer
	}
	p = p[:min(len(p), maxPeekBytes-c.buf.Len())]
	n, err := c.Conn.Read(p)
	if n > 0 {
		c.buf.Write(p[:n])
	}
	return n, err
}

func (c *recordingConn) Write(p []byte) (int, error) { return len(p), nil }

type replayConn struct {
	*recordingConn
	r io.Reader
}

func (c *replayConn) Read(p []byte) (int, error) { return c.r.Read(p) }

type readerFunc func([]byte) (int, error)

func (f readerFunc) Read(p []byte) (int, error) { return f(p) }
