#!/usr/bin/env bash
set -euo pipefail
export COLOR_RESET="\033[0m"
export RED_BG="\033[41m"
export BLUE_BG="\033[44m"

function err {
    echo -e "${RED_BG}$1${COLOR_RESET}"
}

function info {
    echo -e "${BLUE_BG}$1${COLOR_RESET}"
}

if [[ ! -d /mnt/boot ]]; then
    err "No /mnt/boot directory found. Did you run disks.sh first?"
    exit 1
fi

info "Generating NixOS configuration (/mnt/etc/nixos/*.nix) ..."
nixos-generate-config --root /mnt


rm /mnt/etc/nixos/configuration.nix
mv /mnt/etc/nixos/hardware-configuration.nix /mnt/etc/nixos/hosts/quine/

cd /mnt/etc/nixos/hosts/quine

nixos-install --no-root-passwd \
    --option substituters "https://aseipp-nix-cache.global.ssl.fastly.net" \
    --flake '.#quine'

info "Done."
echo
info "You can now reboot into your new NixOS installation."
