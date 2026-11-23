# ts-egress

A Tailscale exit node that decides every outbound TCP, UDP and DNS flow from
rules in the tailnet policy file, and adds API credentials to HTTPS requests
so the client never holds them.

## The problem

You run coding agents, or any workload you only half trust, and you want to
know and limit what it talks to on the internet. Each agent needs a few things:
a package registry, GitHub over HTTPS and SSH, maybe an NTP server. It also
needs API tokens, and anything an agent can read it can also leak.

The usual answers each fall short. A proxy set in `HTTPS_PROXY` is skipped by
any program that ignores the variable. A firewall allowlist on IP addresses
breaks when a CDN moves. A token in the environment is visible to every
process and every prompt.

## Why this exists

Tailscale already knows who each machine is. An exit node already carries all
of a machine's internet traffic. ts-egress puts the policy where the identity
lives:

- Rules are a grant capability (`moot.land/cap/egress`) in the tailnet policy
  file. The egress reads them for each flow with WhoIs, so one policy file
  says both who may reach the egress and what each caller may do through it.
  There is no local rule file and no fallback: a caller without the
  capability gets nothing.
- The decision runs on the egress host, away from the client. Killing or
  tampering with the tenant's Tailscale client cannot loosen the rules. At
  most it cuts the tenant off. Stopping a tenant from going around the exit
  node is the tenant host's job: in this repository `guests/t3-users` does it
  with nftables.
- Secrets stay on the egress. The tenant holds a placeholder such as
  `TS_EGRESS_PLACEHOLDER_GITHUB_TOKEN`, and the egress swaps in the real
  value on the way out, only for the hosts, methods and paths the rule names.

## What it does

ts-egress is the exit node itself: a [tsnet](https://tailscale.com/kb/1244/tsnet)
node that advertises the exit routes, with no tailscaled, kernel routing or
packet redirection on the host. Every TCP and UDP flow a tenant sends through
it arrives in its userspace network stack, and nothing is forwarded by
itself. ts-egress looks up the caller and its rules and then does one of
these:

| Rule kind | Matches | What happens |
|-----------|---------|--------------|
| `dns` | any question on port 53 | `"allow": "rules"` forwards names some other rule covers. Every other name gets REFUSED with Extended DNS Error 15 (Blocked). |
| `tls` | TLS by SNI host and port | Relayed undecrypted. |
| `http` | TLS by SNI, or plain HTTP by Host | Decrypted with a leaf from the egress CA, checked per request (method, path, Host equal to SNI), credentials injected, then sent upstream. |
| `tcp`, `udp` | host or CIDR, and port | Relayed as raw bytes or datagrams. A host matches only addresses the caller resolved through this egress. |
| `deny` | CIDRs or hosts | Refused before any allow rule. |
| `private` | RFC 1918 and ULA ranges | Blocked unless every grant sets `"block": false` or `allow` reopens a range. |

Some destinations are refused whatever the rules say: loopback, link-local
(cloud metadata included), the Tailscale ranges, multicast, broadcast and the
egress host's addresses. UDP to port 443 of an `http` host is refused, so
clients fall back from QUIC to TLS, which the egress can read. Pings to
the internet are dropped.

DNS is answered only on port 53. Tailscale clients normally send DNS to an
exit node over its peer API (DNS over HTTPS), which ts-egress cannot filter,
so it refuses that for every caller. A tenant must use a resolver IP on port
53, such as one in its own `resolv.conf`.

Every flow and every decrypted request writes one JSON line to stdout with
the caller's node name and tags, the verdict, the rule or reason, and byte
counts. A DNS line carries the question.

A rule for one tagged tenant looks like this in the policy file:

```jsonc
{
  "src": ["tag:t3-alice"],
  "dst": ["tag:egress"],
  "app": {
    "moot.land/cap/egress": [{
      "http": [{
        "hosts": ["api.github.com"], "ports": [443], "methods": ["GET", "HEAD"],
        "credentials": [{
          "name": "GITHUB_TOKEN",
          "secret": "op://LLM/github-test/credential",
          "inject": {"header": "Authorization", "format": "Bearer {secret}"}
        }]
      }],
      "tls": [{"hosts": ["registry.npmjs.org", "github.com"], "ports": [443]}],
      "tcp": [{"hosts": ["github.com"], "ports": [22]}],
      "udp": [{"cidrs": ["162.159.200.1/32"], "ports": [123]}],
      "dns": {"allow": "rules"}
    }]
  }
}
```

With that grant, `curl -H "Authorization: Bearer $GITHUB_TOKEN"
https://api.github.com/user` works from the tenant while `$GITHUB_TOKEN`
holds only the placeholder. Sent to another `http` host, whose rule does not
list `GITHUB_TOKEN`, the placeholder is refused with 403. `curl
https://example.com` fails at DNS.

### Credentials

A credential's `secret` is `op://vault/item/[section/]field`, read through
1Password Connect, or `literal:value` for tests. The egress replaces
placeholders in request header values, including inside a Basic
`Authorization` payload. When the inject header is missing it sets it from
`format`. A placeholder in the URL, in a reserved header (`Host`,
`Cookie`, `Expect` and the others that frame, route or forward the
request), or for a credential the rule does not list refuses the request.
No credential may be injected into a reserved header. Bodies are not
searched.

### Broker credentials

A cap object may carry a broker ceiling:
`{"broker": {"groups": ["agents"], "modes": ["inject"]}}`. Several merge by
union, and `inject` is the only mode. For a caller with a ceiling, a
placeholder that no matching `http` rule lists is put to the credential
broker over `brokerSocket`, with the caller's identity (its tags sorted and
joined with `,`, or the owner's login on an untagged node), its groups and
modes, the credential name, the host and the canonical name of the header
that carries the placeholder. The broker answers:

- allow: the egress reads the named 1Password item and the field with the
  given ID, never one matched by label, and replaces the placeholder with
  the raw value, scrubbing included. The tenant writes the header value
  itself, such as `Authorization: Bearer TS_EGRESS_PLACEHOLDER_X`. The
  placeholder may appear only in the header the broker names, which must
  not be reserved. In any other header, alone or as well, the
  request is refused with `placeholder-outside-inject-header` before a
  secret is read.
- pending: 403 with the body `approval-pending <id>`. Retry once a human
  has approved.
- deny, or no usable answer: 403 as for an unknown placeholder.

Each request asks again, so a revoked approval stops the next request. A
static credential of the same name wins: a name any rule in the caller's
caps lists is never put to the broker, and is refused where the matched
rule does not list it. The broker is never asked on plain HTTP or for a
caller without a ceiling. The request audit line has a `broker` entry for
each decision, with `broker-unavailable` as the reason when the broker
could not be asked.

Response headers on a credentialed request have the secret value replaced
with the placeholder. This is a courtesy for servers that echo a token back,
and it does not stop a determined leak. Bodies, compression and streaming
pass through unchanged.

## Quick start

ts-egress is built for NixOS with `modules/services/ts-egress.nix` in this
repository. You need a tailnet where you can edit the policy file, a host or
VM for the egress, and an auth key for the tag it will advertise.

1. In the policy file, define the tag, auto-approve it as an exit node, and
   route the tenant through it:

   ```jsonc
   "tagOwners": { "tag:egress": ["autogroup:admin"] },
   "autoApprovers": { "exitNode": ["tag:egress"] },
   "grants": [
     { "src": ["tag:t3-alice"], "dst": ["autogroup:internet"], "ip": ["*"], "via": ["tag:egress"] }
     // plus a moot.land/cap/egress grant like the one above
   ]
   ```

2. On the egress host:

   ```nix
   {
     imports = [ ./modules/services/ts-egress.nix ];
     services.resolved.enable = true; # the module requires it
     modules.services.ts-egress = {
       enable = true;
       dnsUpstream = "1.1.1.1:53";
       selfAddresses = [ "192.0.2.10" ]; # the host's LAN addresses
     };
   }
   ```

   Put an auth key for `tag:egress` at `/run/host-secrets/ts-authkey`, or
   point `authKeyFile` elsewhere. The node must be tagged, or ts-egress
   stops before it advertises the exit routes: Tailscale serves peer API
   DNS to an untagged node's same-owner devices without asking ts-egress.
   The node logs in with it once and keeps its identity in
   `/var/lib/ts-egress`. The module runs ts-egress as the node
   `egress` and opens its WireGuard port, UDP 41641. The host needs no
   tailscaled.

3. On the tenant, use the egress as its exit node:
   `tailscale set --exit-node=egress`.

From the tenant, `curl https://registry.npmjs.org/` works and
`curl https://example.com/` fails. `journalctl -u ts-egress` on the egress
shows one line for each, the second with `"verdict":"deny"`.

To use `http` rules, also set `ca.enable = true` with the CA certificate and
key as credentials, and have the tenant trust that certificate. To use
`op://` secrets, set `onepassword.url` and supply a Connect token.

## Configuration

The program takes `-config <file.json>` and `-log-level`. The NixOS module
writes the file. It holds no secrets: those arrive as systemd credentials,
read from `$CREDENTIALS_DIRECTORY`, and a credential the file names but
systemd did not provide stops startup.

| Key | Default | Meaning |
|-----|---------|---------|
| `dnsUpstream` | required | Resolver, as `host:port`, for permitted questions. |
| `hostname` | `egress` | Tailscale hostname of the node. |
| `stateDir` | `/var/lib/ts-egress` | Where the node keeps its keys and state. |
| `authKeyCredential` | `ts-authkey` | Name of the auth key credential. Required, though only the first login uses it. |
| `tags` | none | Tags the node advertises. |
| `port` | random | UDP port for WireGuard and peer-to-peer traffic. |
| `self` | none | The host's own addresses, added to the refused set. |
| `identityTTL` | `15s` | How long a caller's rules are cached. A revoked grant takes effect within this time. |
| `peekTimeout` | `5s` | How long to wait for a TLS ClientHello or HTTP request line. |
| `udpIdle` | `1m` | Idle time before a UDP session closes. A DNS session on port 53 closes 150 ms after its last answer instead. |
| `helper` | off | CONNECT helper: `listen`, `hosts`, `ports`. |
| `ca` | off | `certCredential`, `keyCredential`: names of the CA credentials. Enables `http` rules. |
| `onepassword` | off | `url`, `tokenCredential`, `secretTTL`: Connect API for `op://` secrets. |
| `brokerSocket` | off | Absolute path of the credential broker's unix socket. Needs `onepassword`. |

Unknown keys are an error.

The helper is an allow-only CONNECT proxy for Tailscale's control and DERP
hosts. A tenant whose Tailscale client must reach the control plane
before it can use the exit node goes through it. It has no notion of caller,
so it allows only the listed hosts and ports.

## Limitations

- It carries a patch to tsnet and netstack (`patches/`). The patch adds the
  UDP counterpart of `RegisterFallbackTCPHandler`, and makes netstack
  release the address it takes for a flow's destination when the flow ends
  or is refused, also past the in-flight TCP connection limits, and adds a
  switch that drops pings to the internet instead of answering them with
  the host's `ping`. A TCP flow keeps that address until its connection
  has finished closing, not only until its handler returns.
- It carries upstream's open gVisor fix
  ([google/gvisor#15019](https://github.com/google/gvisor/pull/15019) and
  its follow-up 936ad90db, squashed in
  [google/gvisor#17844](https://github.com/google/gvisor/pull/17844);
  `patches/gvisor/`) for a tenant that reopens a tuple the egress holds in
  TIME_WAIT: unpatched gVisor hands such a SYN only to a listener, and
  netstack's forwarder is none, so it drops it until TIME_WAIT ends. Drop
  both patches once tailscale pins a gVisor with both. Not covered by the
  fix: a reopen refused at the in-flight limits loses the old TIME_WAIT,
  and the new connection's ISN is not raised above the old one's.
- Tenants using MagicDNS through the exit node get SERVFAIL: the peer API
  DNS path is refused, see above.
- Rules apply per caller. Two tenants behind one Tailscale node share one
  identity and one rule set.
- `tcp` and `udp` host rules depend on the caller resolving the name through
  the egress first. A hard-coded IP needs a CIDR rule.
- `http` rules mean the tenant must trust the egress CA, and anything pinning
  certificates will fail on decrypted hosts.
- A flow to an allowed name on a port no rule lists has its connection
  closed, which clients report as a generic connection failure.
- One 1Password Connect token serves every caller's `op://` references, so
  the policy file decides which items a caller can use. Give the egress a
  dedicated Connect token and vault.

## Development

The build vendors the modules and patches tsnet and gVisor. Do the same before testing
outside Nix, and again after changing `go.mod`:

```sh
scripts/vendor.sh
go test ./...
nix build .#ts-egress .#checks.x86_64-linux.ts-egress
```

`TestExitNode` runs a tenant through a real tsnet exit node on an offline
tailnet (testcontrol and an in-process DERP), so it needs no network or
privileges.

## License

Apache-2.0. Parts are adapted from
[iron-proxy](https://github.com/ironsh/iron-proxy). See [NOTICE](NOTICE) and
[LICENSE.iron-proxy](LICENSE.iron-proxy).
