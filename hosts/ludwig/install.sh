# ludwig-install: migrate ludwig from TrueNAS CORE to NixOS, one step at a time.
# Packaged into the installer ISO by installer.nix. Run as root on the installer.
# Steps are idempotent where possible and stop at the first unexpected state.
#
#   ludwig-install preflight
#   ludwig-install esp
#   ludwig-install import
#   ludwig-install datasets
#   ludwig-install mount
#   ludwig-install seed
#   ludwig-install install /nix/store/...-nixos-system-ludwig-...
#   ludwig-install bootorder
#   ludwig-install finish
#
# `ludwig-install auto` runs every step without prompts, wipes the staged
# bundle from the TrueNAS ESP, and reboots into NixOS once. The installer
# starts it at boot only when the bundle contains an `autoinstall` marker.
# Without a path, `install` uses the system built into the ISO ($LUDWIG_SYSTEM).

INPUT=${LUDWIG_INPUT:-/root/ludwig-migration}
LOG=/run/ludwig-install.log
HOSTID=ac91e159
TRUENAS_ESP_UUID=d4fb2736-d81c-11ec-bff8-d05099fb0f82
# Partition 1 (TrueNAS swap) and 2 (fast mirror member) of each Samsung SSD.
ESP_UUIDS=(f0616c65-1c4b-11ed-8520-d05099fb0f82 f063b85a-1c4b-11ed-8520-d05099fb0f82)
FAST_UUIDS=(f0677a2b-1c4b-11ed-8520-d05099fb0f82 f069c614-1c4b-11ed-8520-d05099fb0f82)
TANK_UUIDS=(
  cf7d70cf-1c22-11ed-8520-d05099fb0f82
  cf7eae85-1c22-11ed-8520-d05099fb0f82
  cf92350b-1c22-11ed-8520-d05099fb0f82
  cf6b4c9c-1c22-11ed-8520-d05099fb0f82
  cf9d2ca4-1c22-11ed-8520-d05099fb0f82
)
FREEBSD_SWAP=516e7cb5-6ecf-11d6-8ff8-00022d09712b
FREEBSD_ZFS=516e7cba-6ecf-11d6-8ff8-00022d09712b
EFI_SYSTEM=c12a7328-f81f-11d2-ba4b-00a0c93ec93b
ESP_SECTORS=4194304

die() {
  echo "ERROR: $*" >&2
  exit 1
}
info() { echo "==> $*"; }
confirm() {
  if [[ ${LUDWIG_ASSUME_YES:-} == 1 ]]; then
    info "$1 (auto: yes)"
    return 0
  fi
  local reply
  read -r -p "$1 Type YES to continue: " reply
  [[ $reply == YES ]] || die "aborted"
}
part() { echo "/dev/disk/by-partuuid/$1"; }
ptype() { blkid -p -o value -s PART_ENTRY_TYPE "$(part "$1")"; }
keystatus() { zfs get -H -o value keystatus "$1"; }
imported() { zpool list -H -o name "$1" >/dev/null 2>&1; }

check_key() {
  local f=$INPUT/$1.key
  [[ -f $f ]] || die "missing $f"
  tr -d '\n' <"$f" | grep -Eqx '[0-9a-fA-F]{64}' || die "$f is not a 64-character hex key"
}

load_key() {
  local pool=$1 key
  key=$(mktemp)
  tr -d '\n' <"$INPUT/$pool.key" >"$key"
  if [[ $(keystatus "$pool") != available ]]; then
    zfs load-key -n -L "file://$key" "$pool" || die "key for $pool does not match"
    zfs load-key -L "file://$key" "$pool"
  fi
  rm -f "$key"
}

cmd_preflight() {
  [[ $(hostid) == "$HOSTID" ]] || die "hostid is $(hostid), expected $HOSTID"
  for u in "${ESP_UUIDS[@]}" "${FAST_UUIDS[@]}" "${TANK_UUIDS[@]}" "$TRUENAS_ESP_UUID"; do
    [[ -e $(part "$u") ]] || die "partition $u not found"
  done
  for u in "${FAST_UUIDS[@]}" "${TANK_UUIDS[@]}"; do
    [[ $(ptype "$u") == "$FREEBSD_ZFS" ]] || die "$u is not a freebsd-zfs partition"
  done
  for u in "${ESP_UUIDS[@]}"; do
    case $(ptype "$u") in
    "$FREEBSD_SWAP" | "$EFI_SYSTEM") ;;
    *) die "$u is neither TrueNAS swap nor an ESP" ;;
    esac
    [[ $(blockdev --getsz "$(part "$u")") == "$ESP_SECTORS" ]] || die "$u is not 2 GiB"
  done
  check_key fast
  check_key tank
  for f in ssh/ssh_host_ed25519_key ssh/ssh_host_rsa_key ssh/ssh_host_ecdsa_key ups/router_key ups/known_hosts smbpasswd; do
    [[ -s $INPUT/$f ]] || die "missing $INPUT/$f"
  done
  [[ -s /var/lib/tailscale/tailscaled.state ]] || echo "WARNING: no tailscale state; the host will need a fresh login" >&2
  info "pools visible for import:"
  zpool import | grep -E '^ +(pool|state):' || true
  info "BMC watchdog:"
  ipmitool mc watchdog get | grep -E 'Timer (Is|Actions)' || true
  if ipmitool mc watchdog get | grep -q 'Started/Running'; then
    confirm "The BMC watchdog is running and will power-cycle the host."
    ipmitool mc watchdog off
  fi
  info "preflight passed"
}

cmd_esp() {
  local i u dev disk
  for i in 0 1; do
    u=${ESP_UUIDS[$i]}
    dev=$(readlink -f "$(part "$u")")
    disk=/dev/$(lsblk -no PKNAME "$dev")
    [[ $(cat "/sys/class/block/${dev#/dev/}/partition") == 1 ]] || die "$dev is not partition 1"
    if [[ $(ptype "$u") == "$EFI_SYSTEM" ]] && [[ $(blkid -o value -s LABEL "$dev") == "ESP$i" ]]; then
      info "$dev is already ESP$i"
      continue
    fi
    imported fast && die "export fast before changing its disks' partition tables"
    sgdisk -p "$disk"
    confirm "Convert $dev (TrueNAS swap) into ESP$i? Partition 2 is not touched."
    sgdisk --backup="$INPUT/gpt-${disk#/dev/}-$u.bak" "$disk"
    sgdisk --typecode=1:EF00 --change-name="1:ludwig-esp$i" "$disk"
    partx -u "$disk"
    udevadm settle
    wipefs -a "$dev"
    # Drop the gmirror and GELI metadata TrueNAS kept in the last sectors.
    dd if=/dev/zero of="$dev" bs=1M count=1 conv=fsync \
      seek=$(($(blockdev --getsize64 "$dev") / 1048576 - 1))
    mkfs.vfat -F 32 -n "ESP$i" "$dev"
  done
  udevadm settle
  ls -l /dev/disk/by-partlabel/ludwig-esp0 /dev/disk/by-partlabel/ludwig-esp1
}

cmd_import() {
  local pool
  for pool in fast tank; do
    imported "$pool" || zpool import -N -o cachefile=none "$pool" ||
      die "import of $pool failed; do not use -f before checking that TrueNAS is really off"
    load_key "$pool"
  done
  zpool status fast tank
  # Pool properties stay exactly as TrueNAS left them; see hardware-configuration.nix.
  info "local properties (unchanged):"
  zfs get -H -s local -o name,property,value all fast tank || true
}

ensure_dataset() {
  local name=$1
  shift
  zfs list -H "$name" >/dev/null 2>&1 && return 0
  zfs create "$@" "$name"
}

cmd_datasets() {
  [[ $(keystatus fast) == available ]] || die "run import first"
  ensure_dataset fast/encrypted -o canmount=off -o mountpoint=none \
    -o xattr=sa -o acltype=posixacl -o atime=off
  [[ $(zfs get -H -o value encryptionroot fast/encrypted) == fast ]] || die "fast/encrypted is not under the fast key"
  ensure_dataset fast/encrypted/local -o canmount=off -o mountpoint=none
  ensure_dataset fast/encrypted/safe -o canmount=off -o mountpoint=none
  local ds
  for ds in local/root local/nix local/home safe/persist; do
    ensure_dataset "fast/encrypted/$ds" -o mountpoint=legacy
  done
  for ds in local/root local/home; do
    zfs list -H -t snapshot "fast/encrypted/$ds@blank" >/dev/null 2>&1 ||
      zfs snapshot "fast/encrypted/$ds@blank"
  done
  zfs list -r -o name,used,mountpoint,encryptionroot fast/encrypted
}

cmd_mount() {
  mountpoint -q /mnt || mount -t zfs fast/encrypted/local/root /mnt
  mkdir -p /mnt/nix /mnt/home /mnt/persist /mnt/boot /mnt/boot-fallback
  mountpoint -q /mnt/nix || mount -t zfs fast/encrypted/local/nix /mnt/nix
  mountpoint -q /mnt/home || mount -t zfs fast/encrypted/local/home /mnt/home
  mountpoint -q /mnt/persist || mount -t zfs fast/encrypted/safe/persist /mnt/persist
  mountpoint -q /mnt/boot || mount -o umask=0077 /dev/disk/by-partlabel/ludwig-esp0 /mnt/boot
  mountpoint -q /mnt/boot-fallback || mount -o umask=0077 /dev/disk/by-partlabel/ludwig-esp1 /mnt/boot-fallback
  findmnt -R /mnt
}

cmd_seed() {
  mountpoint -q /mnt/persist || die "run mount first"
  local p=/mnt/persist pool t
  install -d -m 0700 "$p/etc/zfs-keys"
  for pool in fast tank; do
    tr -d '\n' <"$INPUT/$pool.key" >"$p/etc/zfs-keys/$pool.key"
    chmod 0400 "$p/etc/zfs-keys/$pool.key"
  done
  install -d -m 0755 "$p/etc/ssh"
  for t in ed25519 rsa ecdsa; do
    install -m 0600 "$INPUT/ssh/ssh_host_${t}_key" "$p/etc/ssh/"
    install -m 0644 "$INPUT/ssh/ssh_host_${t}_key.pub" "$p/etc/ssh/"
  done
  install -d -m 0700 "$p/etc/ups"
  install -m 0600 "$INPUT/ups/router_key" "$p/etc/ups/router_key"
  install -m 0644 "$INPUT/ups/known_hosts" "$p/etc/ups/known_hosts"
  install -d -m 0700 "$p/etc/ludwig-migration"
  install -m 0600 "$INPUT/smbpasswd" "$p/etc/ludwig-migration/smbpasswd"
  if [[ -s /var/lib/tailscale/tailscaled.state ]]; then
    install -d -m 0700 "$p/var/lib/tailscale"
    install -m 0600 /var/lib/tailscale/tailscaled.state "$p/var/lib/tailscale/tailscaled.state"
  fi
  # Fallback login for the installed host if the reused state is ever rejected.
  if [[ -s /run/ludwig-installer/authkey ]]; then
    install -d -m 0700 "$p/etc/tailscale"
    install -m 0600 /run/ludwig-installer/authkey "$p/etc/tailscale/authkey"
  fi
  find "$p/etc" "$p/var" -maxdepth 3 | sort
}

cmd_install() {
  local system=${1:-${LUDWIG_SYSTEM:-}}
  [[ $system == /nix/store/*-nixos-system-ludwig-* ]] || die "usage: ludwig-install install /nix/store/...-nixos-system-ludwig-..."
  nix-store --store /mnt --check-validity "$system" 2>/dev/null ||
    nix-store --check-validity "$system" ||
    die "$system is in neither /mnt nor the installer store; nix copy it first"
  [[ -s /mnt/persist/etc/zfs-keys/fast.key ]] || die "run seed first"
  nixos-install --system "$system" --no-root-passwd --no-channel-copy
  seed_userborn "$system"
}

seed_userborn() {
  # The userborn module refuses to start on an empty /persist/var/lib/userborn
  # (docs/userborn-persistence.md). Seed it with the system's own account set.
  # Password hashes are filled in on first boot, after sops decrypts them.
  local db=/persist/var/lib/userborn exec
  if [[ -s /mnt$db/passwd && -s /mnt$db/group && -s /mnt$db/shadow ]]; then
    info "userborn database already seeded"
    return 0
  fi
  # Store symlinks are absolute, so resolve the unit inside the target.
  exec=$(nixos-enter --root /mnt -c "cat $1/etc/systemd/system/userborn.service" 2>/dev/null |
    sed -nE 's/^ExecStart=(\S+ \S+) \S+$/\1/p')
  [[ $exec == /nix/store/*userborn* ]] || die "cannot read userborn ExecStart from $1"
  install -d -m 0755 "/mnt$db"
  nixos-enter --root /mnt -c "USERBORN_MUTABLE_USERS=false $exec $db" || true
  for f in passwd group shadow; do
    [[ -s /mnt$db/$f ]] || die "userborn did not create $db/$f"
  done
  cut -d: -f1,3,4 "/mnt$db/passwd" | awk -F: '$2 >= 800 && $2 < 30000'
}

entry_for() {
  # Boot entry number whose line matches the given pattern.
  efibootmgr -v | grep -i -- "$1" | head -n1 | sed -E 's/^Boot([0-9A-Fa-f]{4}).*/\1/'
}

cmd_bootorder() {
  local truenas nixos fallback rest
  truenas=$(entry_for "$TRUENAS_ESP_UUID")
  nixos=$(entry_for 'NixOS-boot[^-]')
  fallback=$(entry_for 'NixOS-boot-fallback')
  [[ -n $truenas && -n $nixos && -n $fallback ]] || {
    efibootmgr -v
    die "could not identify the TrueNAS and NixOS boot entries"
  }
  rest=$(efibootmgr | sed -nE 's/^BootOrder: //p' | tr ',' '\n' |
    grep -v -i -x -e "$truenas" -e "$nixos" -e "$fallback" | paste -sd, -)
  # TrueNAS stays the default; NixOS boots once. A power cycle returns to TrueNAS.
  efibootmgr -o "$truenas,$nixos,$fallback${rest:+,$rest}"
  efibootmgr -n "$nixos"
  efibootmgr
}

wipe_bundle() {
  # Overwrite and delete the staged secrets on the TrueNAS ESP.
  local esp f
  esp=$(mktemp -d)
  mount "$(part "$TRUENAS_ESP_UUID")" "$esp"
  if [[ -d $esp/ludwig-installer ]]; then
    find "$esp/ludwig-installer" -type f -print0 |
      while IFS= read -r -d '' f; do
        dd if=/dev/zero of="$f" bs=4096 conv=notrunc,fsync status=none \
          count=$((($(stat -c %s "$f") + 4095) / 4096))
      done
    rm -rf "$esp/ludwig-installer"
  fi
  sync
  umount "$esp"
  rmdir "$esp"
  info "staged bundle wiped from the TrueNAS ESP"
}

cmd_auto() {
  exec > >(tee -a "$LOG") 2>&1
  export LUDWIG_ASSUME_YES=1
  info "unattended install started $(date -Is)"
  cmd_preflight
  cmd_esp
  cmd_import
  cmd_datasets
  cmd_mount
  cmd_seed
  cmd_install
  cmd_bootorder
  wipe_bundle
  info "unattended install finished $(date -Is); rebooting into NixOS once"
  install -D -m 0600 "$LOG" /mnt/persist/var/log/ludwig-install.log
  cmd_finish
  systemctl reboot
}

cmd_finish() {
  umount -R /mnt
  zpool export tank
  zpool export fast
  info "pools exported; reboot to start NixOS once"
}

[[ $EUID -eq 0 ]] || die "run as root"
step=${1:-}
shift || true
case $step in
preflight | esp | import | datasets | mount | seed | install | bootorder | finish | auto)
  "cmd_$step" "$@"
  ;;
*)
  die "usage: ludwig-install preflight|esp|import|datasets|mount|seed|install [system]|bootorder|finish|auto"
  ;;
esac
