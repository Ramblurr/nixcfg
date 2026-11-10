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
VM worker -- host egress policy -- Tailscale HTTPS 8445 -- gateway
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
Only the exact worker gateway address/port bypasses the private-address block.
All other private, link-local, loopback, multicast and Tailscale destinations
are denied, as are IPv6 transition addresses and public ports other than 80/443.
`denyCidrs` can additionally block public destinations. DNS is handled by SmolVM;
the decider evaluates the resulting TCP destination IP, not a guest hostname.

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
  gatewayAddress = "100.64.1.2"; # server's Tailscale IP
  gatewayPort = 8445;
  denyCidrs = [ ]; # include public addresses exposing your unrestricted bb API
};
```

The gateway and egress port settings must agree across hosts. Tailscale must
permit execution hosts to reach the worker port; human devices need the client
port. Tailnet ACLs alone cannot distinguish a local VM from its NAT host, which
is why every VM must also use the host egress policy. Keep the human endpoint
private to Tailscale, or explicitly deny every other address exposing it.

## Manual VM lifecycle

Prerequisite: a SmolVM binary supporting mediated egress (prototype tested with
1.32.2 on Linux/KVM). This module does not install SmolVM or manage VM lifecycle.
Run as the configured host user, after the egress service is active. Substitute
your server's Tailscale IP in the Smolfile. No host directory or socket is mounted.

```bash
mkdir -p ~/.config/bb/isolation/demo
chmod 700 ~/.config/bb/isolation/demo
cat > ~/.config/bb/isolation/demo/Smolfile <<'EOF'
image = "ghcr.io/fgrehm/bb:img-0.11.0-worker-vm"
net = true
cpus = 2
memory = 4096
storage = 40
overlay = 8
[network]
block_private = true
allow_private = ["100.64.1.2/32"]
EOF

smolvm --state-dir "$HOME/.config/bb/isolation/runtime" machine create \
  --name demo --smolfile "$HOME/.config/bb/isolation/demo/Smolfile"

SMOLVM_INTERCEPTOR_TOKEN="$(cat ~/.config/bb/isolation/interceptor-token)" \
  smolvm --state-dir "$HOME/.config/bb/isolation/runtime" machine start \
  --name demo --mediated-egress --egress-interceptor 127.0.0.1:38896

smolvm --state-dir "$HOME/.config/bb/isolation/runtime" machine exec \
  --name demo -- curl -i https://SERVER_TAILSCALE_NAME:8445/api/v1/hosts
# Expected: 403. The same host's human port 8444 must fail to connect.

smolvm --state-dir "$HOME/.config/bb/isolation/runtime" machine stop --name demo
# Repeat the mediated start command to resume. A bare start must fail.
```

Configure bb's direct machine URL to the **worker** HTTPS origin, then use bb's
manual-machine enrollment inside the guest as `developer`. This setting is
separate from the human client URL. Keep enrollment output private: it contains
a one-use credential. Give the guest only its enrollment and model-provider
credentials, not host Git credentials, server state or the interceptor token.
Set guest `BB_DATA_DIR=/home/developer/.config/bb/worker` and
`PI_CODING_AGENT_DIR=/home/developer/.config/bb/pi` for the worker service.
The guest needs to resolve the server's Tailscale name to the permitted IP;
configure guest DNS or an `/etc/hosts` entry when host MagicDNS is unavailable.

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
wrong tokens, oversized frames, IPv4/IPv6 and alternate private destinations.
`bb-module` also checks Quine's existing bb configuration with the new module.

The disposable prototype additionally verified authenticated worker connection,
Pi creating and reading a guest file, denial of the human and backend ports,
client API 403s, broker-down denial and refusal to restart without mediation.
Pi's temporary guest credential file was removed after the smoke test.
