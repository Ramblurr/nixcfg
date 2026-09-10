#!/usr/bin/env bash

#
# 1. Boot target with NixOS installation media
# 2. From console set nixos password: `sudo passwd nixos`
# 3. SSH into the target as nixos user
# 4. Edit your ssh config so that `ssh hostname` works
# 5. Run this script from the private wrapper repo, with the hostname as the first argument
# WARNING: nixos-anywhere repartitions the target disks and installs the system.

set -euo pipefail
umask 077

host="${1:?Usage: deploy-anywhere.sh HOST [BOOTSTRAP_USER]}"

BOOTSTRAP_USER=${2:-nixos}

public_repo=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
host_dir="$public_repo/hosts/$host"
sops_secrets="$host_dir/secrets.sops.yaml"

if [ ! -f "$sops_secrets" ]; then
    echo "No encrypted host secrets found at $sops_secrets" >&2
    exit 1
fi

# Create a temporary directory
temp=$(mktemp -d)

# Function to cleanup temporary directory on exit
cleanup() {
    rm -rf -- "$temp"
}
trap cleanup EXIT

# Create important directories
install -d -m755 "$temp/persist/etc/ssh"
install -d -m700 "$temp/persist/root/"
install -d -m700 "$temp/persist/root/.ssh"
install -d -m755 "$temp/persist/home"

sops -d --extract "['ssh_host_ed25519_key']" "$sops_secrets" >"$temp/persist/etc/ssh/ssh_host_ed25519_key"
sops -d --extract "['ssh_host_ed25519_key_pub']" "$sops_secrets" >"$temp/persist/etc/ssh/ssh_host_ed25519_key.pub"

# Set the correct permissions so sshd will accept the key
chmod 600 "$temp/persist/etc/ssh/ssh_host_ed25519_key"

# Prepare installer access only after the host identity has decrypted successfully.
if [ "$BOOTSTRAP_USER" != "root" ]; then
    ssh-copy-id -f -i ~/.ssh/casey-all.pub "$BOOTSTRAP_USER@$host"
    # The bootstrap username is intentionally expanded on the workstation.
    # shellcheck disable=SC2029
    ssh "$BOOTSTRAP_USER@$host" "sudo cp -r /home/$BOOTSTRAP_USER/.ssh /root/; sudo chown -R root:root /root/.ssh"
fi

nix run github:nix-community/nixos-anywhere/1.10.0 -- --flake ".#$host" --extra-files "$temp" "root@$host"

echo "done"
