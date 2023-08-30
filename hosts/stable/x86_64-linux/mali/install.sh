#!/usr/bin/env bash
set -euo pipefail
export COLOR_RESET="\033[0m"
export RED_BG="\033[41m"
export BLUE_BG="\033[44m"

HOST_DIR=hosts/stable/x86_64-linux/mali

function err {
    echo -e "${RED_BG}$1${COLOR_RESET}"
}

function info {
    echo -e "${BLUE_BG}$1${COLOR_RESET}"
}

function check_key {
    if [[ -f "/mnt/persist/etc/ssh/ssh_host_ed25519_key" ]];
    then
        echo
        info "Here we go..."
        chown -R root:root /mnt/persist/etc/ssh/ssh_host_ed25519_key
        chmod 0600 /mnt/persist/etc/ssh/ssh_host_ed25519_key
    else
        mkdir -p /mnt/persist/etc/ssh
        echo
        echo "Please create /mnt/persist/etc/ssh/ssh_host_ed25519_key"
        echo "It is used to decrypt secrets."
        echo
        read -p "Are you ready? (type 'yes' in capital letters to begin installation): " -r
        if ! [[ $REPLY == "YES" ]];
        then
            err "Aborting"
            exit 1
        fi
        check_key
    fi
}

if [[ "$EUID" -gt 0 ]]; then
    err "Must run as root"
    exit 1
fi

if [[ ! -d /mnt/boot ]]; then
    err "No /mnt/boot directory found. Did you run disks.sh first?"
    exit 1
fi

check_key
rm -f /mnt/etc/nixos/configuration.nix


nixos-install --no-root-passwd --flake '.#mali'

#--option substituters "https://aseipp-nix-cache.global.ssl.fastly.net" \

info "Done."
echo
info "You can now reboot into your new NixOS installation."
