#!/usr/bin/env bash
# Runs TestTPROXYEndToEnd in a throwaway user and network namespace, so it
# needs no root. Run from the module root with go, ip and nft on PATH:
#   nix shell nixpkgs#go nixpkgs#iproute2 nixpkgs#nftables -c scripts/tproxy-e2e.sh
set -euo pipefail

if [[ "${TS_EGRESS_E2E_NETNS:-}" != 1 ]]; then
  exec unshare --user --map-root-user --net env TS_EGRESS_E2E_NETNS=1 "$0" "$@"
fi

ip link set lo up
ip link add dummy0 type dummy
ip link set dummy0 up
ip addr add 100.100.1.1/32 dev dummy0
ip addr add 198.51.100.5/32 dev dummy0
ip rule add fwmark 1 lookup 100
ip route add default dev dummy0
ip route add local 0.0.0.0/0 dev lo table 100
nft -f - <<'NFT'
table ip egress {
  chain out {
    type route hook output priority mangle;
    ip saddr 100.100.1.1 ip daddr != 100.100.1.1 meta mark set 1;
  }
  chain pre {
    type filter hook prerouting priority mangle;
    ip saddr 100.100.1.1 meta l4proto { tcp, udp } tproxy to :15001 meta mark set 1 accept;
  }
}
NFT

exec go test -race -count=1 -run 'TestTPROXYEndToEnd|TestTransparentListenersAsRoot' -v ./internal/egress "$@"
