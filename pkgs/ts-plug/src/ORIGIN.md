# Origin

In-tree fork of [tailscale/ts-plug](https://github.com/tailscale/ts-plug),
upstream commit 14756e741f145d4bcecd725cd022f780f122113d (BSD-3-Clause, see
LICENSE). Imported from the local working branch `t3-unplug-proxy` at
a0b2787, which adds:

- cmd/ts-multi-plug: add local SOCKS5/HTTP proxies and make the command optional
- cmd/ts-multi-plug: set X-Forwarded-Proto for the upstream
- cmd/ts-multi-plug: exit on signals and fail on listener errors
- cmd/ts-multi-plug: half-close CONNECT tunnels on EOF
- cmd/ts-multi-plug: add -exit-node and dial only over the tailnet with it
- cmd/ts-multi-plug: add transparent TCP and UDP capture
- docs: document exit-node egress and transparent capture

Later changes in this tree:

- cmd/ts-multi-plug: answer 403 when the resolver refuses a name by policy
- cmd/ts-multi-plug: proxy HTTP and HTTPS to a unix socket upstream
- cmd/ts-multi-plug: set Tailscale-Node-Name and Tailscale-Node-Tags
- cmd/ts-multi-plug: set identity and X-Forwarded-* headers after hop-by-hop stripping
- cmd/ts-multi-plug: require an absolute path for a unix socket upstream
- cmd/ts-multi-plug: let the local proxies listen on a unix socket
- cmd/ts-multi-plug: add -inherit-output to pass the command's output through

Upstream's docker/, cmd/examples/ and images/ were left out. Edit this tree
directly from now on.
