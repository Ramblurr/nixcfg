#!/usr/bin/env bash

# Seed the Userborn account database of a new impermanent host.
# Writes into ROOT, the staging directory passed to nixos-anywhere --extra-files.
# Does nothing when TOPLEVEL keeps its accounts in /etc.

set -euo pipefail

toplevel="${1:?Usage: userborn-seed.sh TOPLEVEL ROOT}"
root="${2:?Usage: userborn-seed.sh TOPLEVEL ROOT}"

source=$(awk '$2 == "/var/lib/userborn" { print $1 }' "$toplevel/etc/fstab")
if [ -z "$source" ]; then
    exit 0
fi

read -r userborn config _ < <(sed -n 's/^ExecStart=//p' "$toplevel/etc/systemd/system/userborn.service")

# Userborn keeps the modes it finds, so passwd and group must start world-readable.
umask 022
install -d -m755 "$root$source"
# The target sets password hashes from its own secrets on first boot.
"$userborn" <(jq 'del(.users[].hashedPasswordFile)' "$config") "$root$source"
