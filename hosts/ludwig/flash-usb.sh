#!/usr/bin/env bash
# Build the ludwig installer ISO and write it to the USB stick plugged into
# ludwig, from quine, while ludwig still runs TrueNAS. Repeatable: rerun after
# any change to the installer or the ludwig system.
#
#   hosts/ludwig/flash-usb.sh [--dry-run] [--yes] [-- <extra nix build args>]
#
# --dry-run builds, checks the target and stops before anything is written.
#
# LUDWIG_SSH     ssh destination for TrueNAS (default root@192.168.8.4)
# LUDWIG_SSH_OPTS extra ssh options, e.g. "-i ~/.ssh/key -o IdentitiesOnly=yes"
# NIXCFG_PRIVATE nixcfg-private checkout (default ~/nixcfg-private)
#
# Example with an unpushed nixcfg working tree:
#   hosts/ludwig/flash-usb.sh -- --override-input nixcfg git+file://$HOME/nixcfg
set -euo pipefail

host=${LUDWIG_SSH:-root@192.168.8.4}
private=${NIXCFG_PRIVATE:-$HOME/nixcfg-private}
stage=/mnt/fast/ludwig-installer
assume_yes=0
dry_run=0
while [[ $# -gt 0 ]]; do
  case $1 in
  --yes) assume_yes=1 ;;
  --dry-run) dry_run=1 ;;
  --)
    shift
    break
    ;;
  *) break ;;
  esac
  shift
done

read -ra ssh_opts <<<"${LUDWIG_SSH_OPTS:-}"
remote() { ssh -o BatchMode=yes "${ssh_opts[@]}" "$host" "$@" </dev/null; }
info() { echo "==> $*"; }
die() {
  echo "ERROR: $*" >&2
  exit 1
}

info "building the installer ISO from $private"
out=$(nix build --no-link --print-out-paths "$private#ludwig-installer-iso" "$@")
iso=$(echo "$out"/iso/*.iso)
[[ -f $iso ]] || die "no ISO in $out"
size=$(stat -c %s "$iso")
sum=$(sha256sum "$iso" | cut -d' ' -f1)
name=$(basename "$iso")
info "$name: $size bytes, sha256 $sum"

[[ $(remote uname -s) == FreeBSD ]] || die "$host is not running TrueNAS/FreeBSD"

info "looking for the USB stick on $host"
# Exactly one da disk (USB mass storage) that is in no pool and not mounted.
# Pool members appear as gptid labels; glabel maps them back to partitions.
candidates=$(remote 'used=$(zpool status -P 2>/dev/null | awk "/\/dev\// { sub(\"/dev/\", \"\", \$1); print \$1 }")
  for d in $(sysctl -n kern.disks); do
    case $d in da*) ;; *) continue ;; esac
    for g in $used; do
      p=$(glabel status -s | awk -v g="$g" "\$1 == g { print \$3 }")
      case "${p:-$g}" in "$d"*) continue 2 ;; esac
    done
    mount | grep -q "^/dev/$d" && continue
    echo "$d $(diskinfo "$d" | awk "{ print \$3 }") $(diskinfo -v "$d" | awk -F"#" "/Disk descr/ { print \$1 }" | xargs)"
  done')
count=$(grep -c . <<<"$candidates" || true)
[[ $count -eq 1 ]] || die "expected exactly one free USB disk on $host, found $count: $candidates"
read -r disk disk_bytes disk_descr <<<"$candidates"
((disk_bytes > size)) || die "/dev/$disk ($disk_bytes bytes) is smaller than the ISO"
((disk_bytes < 300 * 1024 * 1024 * 1024)) || die "/dev/$disk is larger than 300 GB; not a USB stick?"
info "target: /dev/$disk, $disk_bytes bytes, \"$disk_descr\" (everything on it will be erased)"
if [[ $dry_run == 1 ]]; then
  info "dry run: stopping before upload and write"
  exit 0
fi
if [[ $assume_yes != 1 ]]; then
  read -r -p "Type the disk name ($disk) to write the installer to it: " reply
  [[ $reply == "$disk" ]] || die "aborted"
fi

info "uploading to $host:$stage (resumable)"
remote "mkdir -p $stage && find $stage -name 'ludwig-installer-*.iso' ! -name '$name' -delete"
rsync --partial --inplace --info=progress2 -e "ssh -o BatchMode=yes ${LUDWIG_SSH_OPTS:-}" "$iso" "$host:$stage/$name"
[[ $(remote "sha256 -q $stage/$name") == "$sum" ]] || die "uploaded ISO checksum mismatch"

info "writing /dev/$disk"
remote "dd if=$stage/$name of=/dev/$disk bs=1m && sync"

info "verifying /dev/$disk against the ISO"
blocks=$(((size + 1048575) / 1048576))
[[ $(remote "dd if=/dev/$disk bs=1m count=$blocks 2>/dev/null | head -c $size | sha256 -q") == "$sum" ]] ||
  die "readback of /dev/$disk does not match the ISO"
remote "gpart show $disk" || true
info "done: /dev/$disk holds $name (verified)"
