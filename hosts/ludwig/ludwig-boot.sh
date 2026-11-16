# ludwig-boot: choose between NixOS and the old TrueNAS boot device.
#
#   ludwig-boot status           show the current, next and default entries
#   ludwig-boot nixos-default    boot NixOS by default; TrueNAS stays third
#   ludwig-boot truenas-default  boot TrueNAS by default again (rollback)
#   ludwig-boot truenas-once     boot TrueNAS on the next boot only
#
# Entries are found by label and partition, never by number, so this keeps
# working after firmware or GRUB renumber them.

TRUENAS_ESP_UUID=d4fb2736-d81c-11ec-bff8-d05099fb0f82

die() {
  echo "ERROR: $*" >&2
  exit 1
}

entry_for() {
  efibootmgr -v | grep -i -- "$1" | head -n1 | sed -E 's/^Boot([0-9A-Fa-f]{4}).*/\1/'
}

entries() {
  truenas=$(entry_for "$TRUENAS_ESP_UUID")
  nixos=$(entry_for 'NixOS-boot[^-]')
  fallback=$(entry_for 'NixOS-boot-fallback')
  [[ -n $truenas && -n $nixos && -n $fallback ]] || {
    efibootmgr -v
    die "could not identify the TrueNAS and NixOS boot entries"
  }
}

order() {
  local rest
  rest=$(efibootmgr | sed -nE 's/^BootOrder: //p' | tr ',' '\n' |
    grep -v -i -x -e "$truenas" -e "$nixos" -e "$fallback" | paste -sd, -)
  efibootmgr -o "$1${rest:+,$rest}" >/dev/null
}

status() {
  entries
  efibootmgr | grep -E '^Boot(Current|Next|Order)'
  echo "NixOS $nixos, NixOS fallback $fallback, TrueNAS $truenas"
}

[[ $EUID -eq 0 ]] || die "run as root"
case ${1:-status} in
status) status ;;
nixos-default)
  entries
  order "$nixos,$fallback,$truenas"
  status
  ;;
truenas-default)
  entries
  order "$truenas,$nixos,$fallback"
  status
  ;;
truenas-once)
  entries
  efibootmgr -n "$truenas" >/dev/null
  status
  ;;
*) die "usage: ludwig-boot status|nixos-default|truenas-default|truenas-once" ;;
esac
