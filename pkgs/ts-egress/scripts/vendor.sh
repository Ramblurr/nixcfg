#!/usr/bin/env bash
# Vendors the module and applies patches/ to it, as package.nix does. Run
# from anywhere; go test then builds against the patched tsnet and gVisor.
set -euo pipefail
cd "$(dirname "$0")/.."
go mod vendor
for p in patches/*.patch; do
  patch -p1 -d vendor/tailscale.com <"$p"
done
for p in patches/gvisor/*.patch; do
  patch -p1 -d vendor/gvisor.dev/gvisor <"$p"
done
