package main

import (
	"fmt"
	"io"
	"log/slog"
	"net"
	"slices"

	_ "tailscale.com/feature/ssh"
	"tailscale.com/ssh/tailssh"
)

// serveSSHStdio answers the SSH sessions that ln, from tsnet ListenSSH,
// accepts. A session from a peer holding one of allowTags gets its own TCP
// connection to upstream as its stdin and stdout, whatever command it asks
// for. Every other session is refused before upstream is dialed.
func serveSSHStdio(ln net.Listener, upstream string, allowTags []string, fail func(error)) {
	for {
		conn, err := ln.Accept()
		if err != nil {
			fail(fmt.Errorf("SSH listener: %w", err))
			return
		}
		go bridgeSession(conn.(*tailssh.Session), upstream, allowTags)
	}
}

func bridgeSession(s *tailssh.Session, upstream string, allowTags []string) {
	peer := s.Peer()
	tags := peer.Tags().AsSlice()
	if !slices.ContainsFunc(allowTags, func(tag string) bool { return slices.Contains(tags, tag) }) {
		slog.Warn("SSH: refused", slog.String("peer", peer.Name()), slog.Any("tags", tags))
		fmt.Fprintln(s.Stderr(), "ts-plug: this node serves SSH only to its listed tags")
		s.Exit(1)
		return
	}
	backend, err := net.DialTimeout("tcp", upstream, *flagDialTimeout)
	if err != nil {
		slog.Error("SSH: dial upstream failed", slog.String("upstream", upstream), slog.Any("error", err))
		s.Exit(1)
		return
	}
	defer backend.Close()
	slog.Info("SSH: session", slog.String("peer", peer.Name()))
	go func() {
		io.Copy(backend, s)
		backend.(*net.TCPConn).CloseWrite()
	}()
	if _, err := io.Copy(s, backend); err != nil {
		s.Exit(1)
		return
	}
	s.Exit(0)
}
