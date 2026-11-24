package main

import (
	"context"
	"crypto/ed25519"
	"crypto/rand"
	"encoding/pem"
	"errors"
	"io"
	"net"
	"net/netip"
	"os"
	"path/filepath"
	"strings"
	"sync/atomic"
	"testing"
	"time"

	"golang.org/x/crypto/ssh"
	"tailscale.com/ipn/store/mem"
	"tailscale.com/tsnet"
	"tailscale.com/types/logger"
)

// startDaemon stands in for a builder's nix-daemon --stdio: it answers each
// connection with everything the connection sent, once the sender is done.
func startDaemon(t *testing.T) (addr string, conns *atomic.Int32) {
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { ln.Close() })
	conns = new(atomic.Int32)
	go func() {
		for {
			c, err := ln.Accept()
			if err != nil {
				return
			}
			conns.Add(1)
			go func() {
				defer c.Close()
				in, _ := io.ReadAll(c)
				io.WriteString(c, "daemon:"+string(in))
			}()
		}
	}()
	return ln.Addr().String(), conns
}

func hostKey(t *testing.T, dir string) ssh.PublicKey {
	pub, priv, err := ed25519.GenerateKey(rand.Reader)
	if err != nil {
		t.Fatal(err)
	}
	block, err := ssh.MarshalPrivateKey(priv, "")
	if err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(filepath.Join(dir, "ssh"), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "ssh", "ssh_host_ed25519_key"), pem.EncodeToMemory(block), 0o600); err != nil {
		t.Fatal(err)
	}
	key, err := ssh.NewPublicKey(pub)
	if err != nil {
		t.Fatal(err)
	}
	return key
}

func runOverSSH(ctx context.Context, caller *tsnet.Server, dst netip.Addr, key ssh.PublicKey, stdin string) (string, error) {
	addr := netip.AddrPortFrom(dst, 22).String()
	conn, err := caller.Dial(ctx, "tcp", addr)
	if err != nil {
		return "", err
	}
	defer conn.Close()
	c, chans, reqs, err := ssh.NewClientConn(conn, addr, &ssh.ClientConfig{
		User:              "nix",
		HostKeyCallback:   ssh.FixedHostKey(key),
		HostKeyAlgorithms: []string{ssh.KeyAlgoED25519},
	})
	if err != nil {
		return "", err
	}
	client := ssh.NewClient(c, chans, reqs)
	defer client.Close()
	sess, err := client.NewSession()
	if err != nil {
		return "", err
	}
	defer sess.Close()
	sess.Stdin = strings.NewReader(stdin)
	out, err := sess.Output("nix-daemon --stdio")
	return string(out), err
}

// A builder's node serves SSH without client keys, under its own host key.
// A caller with a listed tag gets the daemon on its session's stdio; any
// other caller is refused before the daemon sees a connection.
func TestSSHStdioServesOnlyListedTags(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()
	controlURL := startControl(t, nil).HTTPTestServer.URL
	daemon, conns := startDaemon(t)

	dir := filepath.Join(t.TempDir(), "builder")
	key := hostKey(t, dir)
	builder := &tsnet.Server{
		Dir:        dir,
		ControlURL: controlURL,
		Hostname:   "builder",
		Store:      new(mem.Store),
		Ephemeral:  true,
		Logf:       logger.Discard,
	}
	t.Cleanup(func() { builder.Close() })
	st, err := builder.Up(ctx)
	if err != nil {
		t.Fatal(err)
	}
	ln, err := builder.ListenSSH(":22")
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	go serveSSHStdio(ln, daemon, []string{"tag:moot-workstead"}, func(error) {})

	workstead, _ := startNode(t, ctx, controlURL, "workstead", "tag:moot-workstead")
	outstead, _ := startNode(t, ctx, controlURL, "outstead", "tag:moot-outstead")

	out, err := runOverSSH(ctx, workstead, st.TailscaleIPs[0], key, "build")
	if err != nil {
		t.Fatalf("workstead: %v", err)
	}
	if out != "daemon:build" {
		t.Errorf("workstead got %q, want %q", out, "daemon:build")
	}

	out, err = runOverSSH(ctx, outstead, st.TailscaleIPs[0], key, "build")
	var exit *ssh.ExitError
	if !errors.As(err, &exit) || exit.ExitStatus() == 0 {
		t.Errorf("outstead: err = %v, want a nonzero exit", err)
	}
	if out != "" {
		t.Errorf("outstead got %q, want nothing", out)
	}
	if n := conns.Load(); n != 1 {
		t.Errorf("daemon saw %d connections, want 1", n)
	}
}
