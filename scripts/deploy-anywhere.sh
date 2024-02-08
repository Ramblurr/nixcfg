#!/usr/bin/env bash

set -ex

host="$1"

host_dir=$(find hosts/ -type d -name "$host")

if [ ! -d "$host_dir" ]; then
  echo "No host directory found for $host"
  exit 1
fi

ssh-copy-id -f -i ~/.ssh/casey-all.pub "nixos@$host"
ssh "nixos@$host" "sudo cp -r /home/nixos/.ssh /root/; sudo chown -R root:root /root/.ssh"


# Create a temporary directory
temp=$(mktemp -d)

# Function to cleanup temporary directory on exit
cleanup() {
  rm -rf "$temp"
}
trap cleanup EXIT

# Create important directories
install -d -m755 "$temp/persist/etc/ssh"
install -d -m700 "$temp/persist/root/"
install -d -m700 "$temp/persist/root/.ssh"
install -d -m755 "$temp/persist/home"



sops_secrets="$host_dir/secrets.sops.yaml"

sops -d --extract "['ssh_host_ed25519_key']" "$sops_secrets"  > "$temp/persist/etc/ssh/ssh_host_ed25519_key"
sops -d --extract "['ssh_host_ed25519_key_pub']" "$sops_secrets" > "$temp/persist/etc/ssh/ssh_host_ed25519_key.pub"

# Set the correct permissions so sshd will accept the key
chmod 600 "$temp/persist/etc/ssh/ssh_host_ed25519_key"


# WORKAROUND: https://github.com/nix-community/nixos-anywhere/issues/260
ssh root@$host "nix-env -iA nixos.rsync"

nix run github:nix-community/nixos-anywhere -- --flake ".#$host" --extra-files "$temp"  "root@$host"

echo "done"
exit 0

zpool import -l rpool
mount rpool/encrypted/local/root /mnt/ -o X-mount.mkdir -o defaults -t zfs
mount /dev/disk/by-partlabel/disk-vda-ESP /mnt/boot -t vfat -o defaults -o X-mount.mkdir
mount rpool/encrypted/local/home /mnt/home -o X-mount.mkdir -o defaults -t zfs
mount rpool/encrypted/local/nix /mnt/nix -o X-mount.mkdir -o defaults -t zfs
mount rpool/encrypted/safe/persist /mnt/persist -o X-mount.mkdir -o defaults -t zfs

umount /mnt/persist
umount /mnt/nix
umount /mnt/home
umount /mnt/boot
umount /mnt
zpool export rpool
