# ts-plug Guide

**ts-plug** wraps your local server and exposes it on your tailnet with automatic TLS and DNS.

## Overview

ts-plug is a reverse proxy that:
- Starts and manages your upstream server process
- Connects to your tailnet
- Provides automatic HTTPS with valid TLS certificates
- Optionally exposes services publicly via Tailscale Funnel
- Supports HTTP, HTTPS, and DNS proxying

## Installation

Build from source:
```sh
make ts-plug
```

Install to $GOPATH/bin:
```sh
make install
```

## Basic Usage

The basic pattern is:
```sh
ts-plug [flags] -- [your-server-command]
```

Everything after `--` is treated as the command to run.

### Simple Examples

Run a Python HTTP server on your tailnet:
```sh
ts-plug -hostname myserver -- python -m http.server 8080
```

Run a Node.js app:
```sh
ts-plug -hostname api -- node server.js
```

Run a Go server:
```sh
ts-plug -hostname webapp -- go run main.go
```

## How It Works

```
┌─────────────────────────────────────────────────────────┐
│  Your Local Machine                                     │
│                                                         │
│  ┌──────────────┐          ┌──────────────┐             │
│  │  ts-plug     │  starts  │ Your Server  │             │
│  │              │ ──────>  │ localhost:80 │             │
│  └──────┬───────┘          └──────────────┘             │
│         │                                               │
└─────────┼───────────────────────────────────────────────┘
          │ Tailscale (encrypted)
          │
┌─────────┼───────────────────────────────────────────────┐
│  Your tailnet                                           │
│         │                                               │
│         │    ┌──────────────┐    ┌──────────────┐       │
│         └──> │   HTTPS:443  │───>│ Team Members │       │
│              │   (with TLS) │    │   Devices    │       │
│              └──────────────┘    └──────────────┘       │
└─────────────────────────────────────────────────────────┘
```

ts-plug:
1. Starts your upstream server process
2. Connects to your tailnet
3. Provisions TLS certificates automatically
4. Listens for connections on your tailnet
5. Reverse proxies traffic to your local server

## Configuration Flags

### Required

- Command after `--` - The server command to execute

### Network

- `-hostname` / `-hn` - Hostname on your tailnet (default: "tsmultiplug")
  ```sh
  ts-plug -hostname myapp -- python app.py
  # Access at: https://myapp.tailnet-name.ts.net
  ```

- `-dir` - Directory to store Tailscale state (default: ".data")
  ```sh
  ts-plug -dir /var/lib/tsplug -hostname api -- ./server
  ```

- `-advertise-tags` - Comma-separated ACL tags for the node, such as
  `tag:web,tag:prod`. Required when `TS_AUTHKEY` holds an OAuth client secret.

### Listeners

By default, ts-plug enables HTTPS on port 443 proxying to localhost:8080.

#### HTTP

- `-http` - Enable HTTP listener (default port mapping: 80:8080)
  ```sh
  # Enable HTTP, proxy port 80 to localhost:8080
  ts-plug -http -hostname web -- python -m http.server 8080
  ```

- `-http-port` - Customize HTTP port mapping
  ```sh
  # Listen on port 8000, proxy to localhost:3000
  ts-plug -http-port 8000:3000 -hostname web -- node server.js

  # Listen and proxy both on port 9000
  ts-plug -http-port 9000 -hostname web -- ./server
  ```

#### HTTPS

- `-https` - Enable HTTPS listener (default port mapping: 443:8080)
  ```sh
  ts-plug -https -hostname secure -- python -m http.server 8080
  ```

- `-https-port` - Customize HTTPS port mapping
  ```sh
  # Listen on port 8443, proxy to localhost:3000
  ts-plug -https-port 8443:3000 -hostname web -- node server.js
  ```

#### Address Upstream

`-http-port` and `-https-port` also take `in:ip:port`, which proxies the
listener to an HTTP server at a numeric address instead of localhost, such as
a VM on a link that only ts-plug's network namespace reaches:

```sh
ts-plug -https-port 443:10.77.0.2:3780 -hostname vm
```

#### Unix Socket Upstream

`-http-port` and `-https-port` also take `in:unix:<path>`, which proxies the
listener to an HTTP server on a unix socket instead of a localhost port:

```sh
ts-plug -https-port 443:unix:/run/myapp/ui.sock -hostname myapp -- ./server
```

With a socket the upstream opens no TCP port, so filesystem permissions
decide which local users can reach it. Give the socket a mode or group that
only ts-plug's user can open, and no other process on the host can bypass
ts-plug or send forged identity headers.

The path must be absolute. ts-plug refuses a relative path and an abstract
socket (`@name`) at startup, since an abstract socket has no permissions and
any local process can connect to it. Every directory on the path must also be
closed to other users: one they can write to lets them replace the socket,
and one they can traverse lets them reach it whenever its own mode allows.
`-dns-port` takes ports only.

#### DNS

- `-dns` - Enable DNS listener (default port mapping: 53:53)
  ```sh
  ts-plug -dns -hostname dns -- pihole-FTL
  ```

- `-dns-port` - Customize DNS port mapping
  ```sh
  # Forward DNS from port 53 to localhost:5353
  ts-plug -dns-port 53:5353 -hostname resolver -- dnsmasq
  ```

### Public Access

- `-public` - Enable Tailscale Funnel for public HTTPS access
  ```sh
  ts-plug -public -hostname demo -- python -m http.server 8080
  # Now accessible from the public internet!
  ```

  This is perfect for:
  - Webhook testing
  - Demo sites
  - Temporary public APIs
  - Sharing work with clients

### Debugging

- `-log` - Set log level (debug, info, warn, error)
  ```sh
  ts-plug -log debug -hostname myapp -- node server.js
  ```

- `-debug-tsnet` - Enable verbose tsnet.Server logging
  ```sh
  ts-plug -debug-tsnet -hostname myapp -- ./server
  ```

## Advanced Usage

### Multiple Listeners

Enable multiple protocols simultaneously:
```sh
# HTTP, HTTPS, and DNS
ts-plug -http -https -dns -hostname multi -- ./server
```

### Custom Port Mappings

Map different ports for each protocol:
```sh
ts-plug \
  -http-port 80:3000 \
  -https-port 443:3000 \
  -hostname myapp \
  -- node server.js
```

### Environment Detection

Your server can detect when it's running under ts-plug:
```sh
if [ "$TSPLUG_ACTIVE" = "1" ]; then
  echo "Running behind ts-plug!"
fi
```

```python
import os
if os.getenv('TSPLUG_ACTIVE') == '1':
    print("Running behind ts-plug!")
```

### Command Output

ts-plug logs each line the command writes, prefixed, on its own stderr.
With `-inherit-output` the command writes to ts-plug's stdout and stderr
directly, so a service manager keeps its lines as written, such as a JSON
log on stdout. ts-plug's own log stays on stderr.

### Local Proxies

`-socks5 <addr>` and `-http-proxy <addr>` start local proxies that dial
through this node, so local clients reach the tailnet with the node's
identity. The address is a TCP `host:port` or `unix:<path>`:

```sh
ts-plug -hostname app -https-port 443:unix:/run/app/ui/ui.sock \
  -http-proxy unix:/run/app/ui/proxy.sock -- ./server
```

A TCP proxy on loopback lets every local user dial as the node. A socket
in a directory only ts-plug's user can traverse limits it to that user and
the command ts-plug starts. The path must be absolute, as for a unix socket
upstream.

### Egress Through an Exit Node

ts-plug can carry all outbound traffic of a network namespace over the
tailnet, with internet traffic leaving through an exit node.

- `-exit-node <ip or name>` selects the exit node after the node starts and
  exits non-zero if it does not resolve to a peer. With it, the SOCKS5 and
  HTTP proxies dial only through the tailnet. A dial that cannot go through
  the exit node fails; it never falls back to the host network. Without the
  flag, ts-plug clears any exit node a previous run left in the state
  directory.
- `-dns-resolver <ip:port>` is required with `-exit-node`. The proxies
  resolve names outside the tailnet by sending DNS to this numeric address
  over the tailnet, so the query leaves through the exit node as port 53
  traffic. Tailnet names still resolve from the netmap, and a tailnet name
  missing from it is never sent to this resolver. When the resolver refuses
  a name with Extended DNS Error 15 (Blocked) or 18 (Prohibited), the HTTP
  proxy answers 403; other failures answer 502. Both carry a `Proxy-Status`
  header (RFC 9209) with the DNS answer, for example
  `ts-plug; error=dns_error; rcode="REFUSED"; info-code=15`.
- `-transparent-tcp <addr>` accepts TCP that nftables steers to it with
  TPROXY or REDIRECT, and dials each connection's original destination.
- `-transparent-udp <addr>` receives UDP that nftables steers to it with
  TPROXY. Each (source, destination) pair gets its own association, closed
  after a minute without packets. Replies go back from the original
  destination address.

Transparent flows always go through the tailnet, with or without
`-exit-node`. Without one, only tailnet destinations work. Captured flows to
100.100.100.100 are refused, because this node's resolver would forward them
to the exit node's own DNS proxy.

The listeners need `CAP_NET_ADMIN` in the namespace for `IP_TRANSPARENT`.

Run ts-plug as its own user and capture every other user's TCP and UDP. This
ruleset sends everything that is not local to ts-plug on 127.0.0.1:15001 and
rejects IPv6 so clients use IPv4:

```nft
table inet tsplug {
	chain output {
		type route hook output priority mangle; policy accept;
		meta skuid "ts-plug" return
		fib daddr type local return
		meta nfproto ipv6 reject
		meta l4proto { tcp, udp } meta mark set 0x1
	}
	chain prerouting {
		type filter hook prerouting priority mangle; policy accept;
		iifname != "lo" return
		meta mark 0x1 meta l4proto { tcp, udp } tproxy ip to 127.0.0.1:15001 accept
		meta mark 0x1 meta l4proto tcp reject with tcp reset
		meta mark 0x1 reject
	}
}
```

Marked packets must route back into the namespace:

```sh
ip rule add fwmark 0x1 lookup 100
ip route add local 0.0.0.0/0 dev lo table 100
```

Start ts-plug with `-transparent-tcp 127.0.0.1:15001 -transparent-udp
127.0.0.1:15001`. The namespace still needs a default route so clients can
send to internet addresses at all. DNS needs no special handling: point
`resolv.conf` at a public resolver and its queries are captured like any
other UDP or TCP.

REDIRECT works for TCP instead of TPROXY (UDP needs TPROXY):

```nft
table ip tsplug-nat {
	chain output {
		type nat hook output priority dstnat; policy accept;
		meta skuid "ts-plug" return
		fib daddr type local return
		meta l4proto tcp redirect to :15002
	}
}
```

With `-transparent-tcp 127.0.0.1:15002`, ts-plug reads the original
destination with `SO_ORIGINAL_DST`.

If ts-plug's own connection to Tailscale must use an HTTP CONNECT proxy, set
these in its environment:

```sh
HTTPS_PROXY=http://192.0.2.10:3128  # an IP, so ts-plug needs no DNS
TS_FORCE_NOISE_443=true             # skip the plain HTTP control attempt
TS_NO_LOGS_NO_SUPPORT=true          # skip log upload to log.tailscale.com
```

Control and DERP then use `CONNECT controlplane.tailscale.com:443` and
`CONNECT <derp host>:443`. Leave `HTTP_PROXY` unset: control's port 80
attempt would send a plain HTTP request to the proxy instead of CONNECT.
WireGuard UDP to peers does not use the proxy and falls back to DERP when
blocked.

## Security Considerations

### Automatic TLS

ts-plug automatically provisions valid TLS certificates for your tailnet hostname. No configuration needed.

### User Identity Headers

When requests come through ts-plug, these headers are added:
- `Tailscale-User-Login` - User's login email
- `Tailscale-User-Name` - User's display name
- `Tailscale-User-Profile-Pic` - URL to user's profile picture
- `Tailscale-Node-Name` - The calling node's MagicDNS name, such as
  `laptop.tailnet-name.ts.net`
- `Tailscale-Node-Tags` - The calling node's tags, comma-separated, such as
  `tag:ci,tag:prod`

ts-plug sets every one of these on every request, replacing any value the
client sent, and leaves a header empty when it does not apply or the WhoIs
lookup fails. It sets them after removing hop-by-hop headers, so a client
cannot strip them by naming them in `Connection`. A tagged node has no user,
so its `Tailscale-User-*` headers are empty and `Tailscale-Node-Tags` names
it. A user's device has empty `Tailscale-Node-Tags`.

Your server can use these for authentication:

```python
@app.route('/api/whoami')
def whoami():
    return {
        'login': request.headers.get('Tailscale-User-Login'),
        'name': request.headers.get('Tailscale-User-Name'),
        'picture': request.headers.get('Tailscale-User-Profile-Pic')
    }
```

ts-plug also replaces `X-Forwarded-For` with the caller's tailnet address,
`X-Forwarded-Host` with the host the caller requested, and
`X-Forwarded-Proto` with `http` or `https`. It drops any `Forwarded` header.

### Network Isolation

Services are only accessible to devices on your tailnet (unless `-public` is used).

## Use Cases

### Local Development Sharing

Share your dev server with teammates:
```sh
ts-plug -hostname dev-alice -- npm run dev
# Tell your teammate to visit: https://dev-alice.tailnet.ts.net
```

### Webhook Testing

Test webhooks without ngrok:
```sh
ts-plug -public -hostname webhook-test -- python webhook_server.py
# Use the public URL in GitHub/Stripe/etc webhook settings
```

### Container Deployment

Use as an entrypoint to eliminate sidecar containers:
```dockerfile
COPY ts-plug /usr/local/bin/
ENTRYPOINT ["ts-plug", "-hostname", "myapp", "--"]
CMD ["python", "app.py"]
```

See [docker.md](./docker.md) for detailed examples.

### Multi-Protocol Services

Run Pi-hole with both DNS and HTTP:
```sh
ts-plug \
  -dns \
  -http \
  -hostname pihole \
  -- pihole-FTL
```

## Troubleshooting

### Port Already in Use

If you get "address already in use", another process is listening on the configured port:
```sh
# Check what's using port 8080
lsof -i :8080

# Use a different port
ts-plug -https-port 443:3000 -hostname myapp -- node server.js
```

### Connection Refused

If ts-plug can't connect to your server:
1. Verify your server is listening on the correct port
2. Make sure it's listening on `0.0.0.0` or `127.0.0.1`, not a specific IP
3. Check logs with `-log debug`

### Tailscale Authentication

First run will prompt you to authenticate with Tailscale:
```sh
ts-plug -hostname test -- python -m http.server 8080
# Follow the URL to authenticate
```

State is saved in the `-dir` location (default: `.data/`)

To log in unattended, put an auth key in `TS_AUTHKEY`. An OAuth client secret
(`tskey-client-...`) works too: ts-plug mints a single-use auth key from it
for the tags in `-advertise-tags`, and strips `TS_AUTHKEY` from any wrapped
child command's environment. That key is ephemeral and not preauthorized unless
the secret carries `?ephemeral=false&preauthorized=true`:
```sh
TS_AUTHKEY='tskey-client-...?ephemeral=false&preauthorized=true' \
  ts-plug -hostname api -advertise-tags tag:api -https-port 443:8080
```

## Examples

### Next.js Development
```sh
ts-plug -hostname nextjs-dev -https-port 443:3000 -- npm run dev
```

### Django Application
```sh
ts-plug -hostname django -https-port 443:8000 -- python manage.py runserver
```

### Static Site
```sh
ts-plug -public -hostname my-site -- python -m http.server 8080
```

### API with Custom Domain
```sh
ts-plug -hostname api-v1 -https-port 443:5000 -- flask run
```

## Comparison with ts-unplug

| Feature | ts-plug | ts-unplug |
|---------|---------|-----------|
| Direction | Local → tailnet | tailnet → Local |
| Use Case | Share local services | Access remote services |
| Starts Process | Yes | No |
| TLS | Automatic | Proxies existing |
| Public Access | Optional | No |

## See Also

- [ts-unplug Guide](./ts-unplug.md) - Access remote services locally
- [Use Cases](./use-cases.md) - Real-world patterns
- [Docker Examples](./docker.md) - Container integration
- [Main README](../README.md) - Quick start guide
