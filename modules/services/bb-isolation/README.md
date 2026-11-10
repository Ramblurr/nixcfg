# bb worker isolation

This module adds two independent roles. Quine enables both. A different server
host can enable just `gateway`; local, LAN and cloud execution hosts enable
just `egress`, pointing at the server's Tailscale IP. No guest Tailscale identity
is required. There is no launcher or bb machine-provider plugin yet.
Quine's address comes from `config.site.hosts.quine.tailscaleIPv4`, defined once
in the shared site inventory (`config/site.nix`).

```text
Phone / desktop -- Tailscale HTTPS 8444 ------------------+
                                                         v
                                                 bb loopback backend
                                                         ^
VM worker -- 192.0.2.1:8445 -- host broker -- Tailscale HTTPS 8445 -- gateway
                |
                +-- public TCP 80/443 (models, packages, Git HTTPS)
                +-- deny other destinations and ports
```

The gateway is a dedicated loopback Caddy listener. Tailscale Serve terminates
HTTPS and forwards only the worker port to it. Exact method/path allowlists
forward enrollment, authenticated worker sessions/WebSocket, skill downloads
and plugin artifacts. Everything else returns 403, including `/api/v1`, client
WebSockets, enrollment-key minting, server moves and server-side tool calls.
The original backend stays on loopback. The gateway rewrites upstream Host so
bb's ordinary browser-origin checks remain enabled.

The egress service implements SmolVM's authenticated SMOLMEG2 protocol on host
loopback. The token is generated at runtime under `~/.config/bb/isolation`, mode
0600, and is supplied to SmolVM by the trusted operator. It never enters the VM.
All private, link-local, loopback, multicast and Tailscale destinations are
denied, as are IPv6 transition addresses and public ports other than 80/443.
`denyCidrs` can additionally block public destinations. DNS is handled by SmolVM;
the decider evaluates the resulting TCP destination IP, not a guest hostname.

### Host services through placeholders

SmolVM runs with `SMOLVM_EGRESS_FLOOR=strict`. That floor refuses every private
and CGNAT (Tailscale) destination before the decider is asked, so a VM cannot
dial the gateway's real Tailscale IP. `allow_private` cannot reopen it under
the strict floor, and it is not port-scoped in any mode.

Instead, guests reach host-side services through placeholder addresses in
192.0.2.0/24 (TEST-NET-1: not floored, never routed). `hostServices` lists every
such service. The default entry is the worker gateway: `192.0.2.1:8445` goes to
`gatewayAddress:gatewayPort`. Each VM gets one redirect rule per selected entry
and a deny rule for the rest of 192.0.2.0/24:

```json
[{"action": "redirect", "transport": "tcp", "cidr": "192.0.2.1/32", "ports": {"start": 8445, "end": 8445}},
 {"action": "deny", "cidr": "192.0.2.0/24"}]
```

SmolVM refuses any start of a machine with a redirect rule unless the start is
mediated. The broker answers redirect only for listed placeholders, connects to
the target, writes the flow's initial bytes, then carries the stream both ways.
If the target is down, the flow is denied. The guest resolves the gateway's real
name (`gatewayHostname`) to the placeholder, so TLS still verifies that name.
The real Tailscale IP is never dialed by the VM.

The broker does not know which VM selected which entry: the per-VM deny rule
does that. Any other VM bound to this broker without such a rule can reach every
listed placeholder. Add entries only for services that every mediated VM on the
host may reach, or use a separate broker.

The module also writes `/etc/bb-isolation/mediation.json` (broker address,
token file path, service names, placeholders and hostnames; no token). Sandbox
reads it through `:mediation` in its user `config.edn` and selects entries with
`:network {:host-services ["bb-worker"]}`.

## Configure roles

On the bb server host:

```nix
services.bbIsolation.gateway.enable = true;
# services.bb must be enabled, bindHost = "127.0.0.1", openFirewall = false.
# gateway.backendPort, listenPort and httpsPort are configurable.
```

On each execution host:

```nix
services.bbIsolation.egress = {
  enable = true;
  user = "developer";
  gatewayAddress = "100.64.1.2"; # server's Tailscale IP, dialed by the broker only
  gatewayHostname = "server.example.ts.net"; # TLS name guests resolve to 192.0.2.1
  gatewayPort = 8445;
  # hostServices defaults to the worker gateway. Append other host services here.
  denyCidrs = [ ]; # include public addresses exposing your unrestricted bb API
};
```

The gateway and egress port settings must agree across hosts. Tailscale must
permit execution hosts to reach the worker port; human devices need the client
port. Tailnet ACLs alone cannot distinguish a local VM from its NAT host, which
is why every VM must also use the host egress policy. Keep the human endpoint
private to Tailscale, or explicitly deny every other address exposing it.

## Manual VM lifecycle

Prerequisite: a SmolVM 1.34.0 or newer local API with mediated egress, on
Linux/KVM, with `SMOLVM_EGRESS_FLOOR=strict`. This module does not install
SmolVM or manage VM lifecycle. Sandbox does this for you: set `:mediation
"/etc/bb-isolation/mediation.json"` and select `bb-worker` in its configuration.
Without Sandbox, use the local HTTP API as the configured host user, after the
egress service is active. Redirect rules are not available in a Smolfile or
on the `machine create` command line in 1.34.0.

```text
POST /api/v1/machines
{"name": "demo", "image": "...", "network": true, "networkBackend": "virtio-net",
 "egressRules": [
   {"action": "redirect", "transport": "tcp", "cidr": "192.0.2.1/32", "ports": {"start": 8445, "end": 8445}},
   {"action": "deny", "cidr": "192.0.2.0/24"}]}

POST /api/v1/machines/demo/start
{"egressInterceptor": {"address": "127.0.0.1:16896", "token": "<interceptor-token>", "mediated": true}}
```

Send the token in a request body only, never in a command-line argument or a
guest exec argument. Then, inside the guest:

```bash
echo "192.0.2.1 SERVER_TAILSCALE_NAME" >> /etc/hosts
curl -i https://SERVER_TAILSCALE_NAME:8445/api/v1/hosts
# Expected: 403 from the gateway. Port 8444, the real Tailscale IP and
# every other private address must fail to connect.
```

Stop and start again with the same start body. A start without it must fail.

Configure bb's direct machine URL to the **worker** HTTPS origin, then use bb's
manual-machine enrollment inside the guest as `developer`. This setting is
separate from the human client URL. Keep enrollment output private: it contains
a one-use credential. Give the guest only its enrollment and model-provider
credentials, not host Git credentials, server state or the interceptor token.
Set guest `BB_DATA_DIR=/home/developer/.config/bb/worker` and
`PI_CODING_AGENT_DIR=/home/developer/.config/bb/pi` for the worker service.
The guest resolves the server's Tailscale name to the placeholder (Sandbox
writes the `/etc/hosts` entry on each boot).

The image does not bundle bb. The prototype found that bb 0.49.0's Nix-packaged
download artifact contains Nix-store Node shebangs that fail in this non-Nix
image. A temporary guest-only rewrite to `/usr/bin/env node`, followed by
installer retry, worked. Disable daemon auto-updates; they would overwrite that
fix. A portable, pinned worker image/package is still required before automated
provisioning. No launcher or image builder is included in this change.

## Boundary and limitations

- Guest root controls its worker and can steal its model and bb worker
  credentials, tamper with results, and destroy its own files. Keep workspaces
  and credentials separate per VM. Read-write host mounts extend that boundary
  to their contents and are deliberately absent from the example.
- The gateway protects the client API; it does not make bb a hardened
  multi-tenant service. Worker WebSocket messages still reach bb's own handlers
  and installed plugin callbacks. Those remain trusted server attack surface.
- bb's authenticated skill registry is shared across workers. Treat all
  server-published skills as worker-readable, not confidential per-project data.
- Server-side agent/plugin tool calls are blocked. Ordinary Pi filesystem and
  shell tools work; bb tools that call back to the server may fail with 403.
- Broker failure denies **new** connections. Existing allowed TCP flows remain
  open. Restart VMs to revoke established flows after changing policy.
- The VM's mediated-mode requirement survives stop/start. Use this mode from
  its very first start; starting a new VM without mediation leaves it unprotected.
  Trusted host operators can change policy or bypass it; guest root cannot.
- Public HTTPS is allowed for model/package access, so this is not a data-loss
  prevention policy. Do not give guests an external proxy with privileged access
  back to the client API, host credentials, or access to the host control plane.

## Verification

`checks.<system>.bb-isolation` evaluates disabled/server/worker/combined roles
and runs the real Caddy proxy against a test backend plus actual SMOLMEG2 socket
tests. It covers forbidden methods, routes and encoded traversal attempts,
wrong tokens, oversized frames, IPv4/IPv6 and alternate private destinations,
and placeholder redirects (initial bytes first, unknown placeholders and dead
targets denied).
`bb-module` also checks Quine's existing bb configuration with the new module.

The disposable prototype additionally verified authenticated worker connection,
Pi creating and reading a guest file, denial of the human and backend ports,
client API 403s, broker-down denial and refusal to restart without mediation.
Pi's temporary guest credential file was removed after the smoke test.
