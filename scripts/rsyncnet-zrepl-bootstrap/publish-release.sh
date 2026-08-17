#!/bin/sh
set -eu
umask 077
PATH=/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin
export PATH

SOURCE_DIR=$(CDPATH='' cd -- "$(dirname "$0")" && pwd)
# shellcheck source=/dev/null
. "$SOURCE_DIR/lib.sh"

usage() {
  printf 'usage: %s BUNDLE_ID\n' "$0" >&2
  exit 64
}

mode_of() {
  stat -f '%Lp' "$1" 2>/dev/null || stat -c '%a' "$1" 2>/dev/null
}

owner_of() {
  stat -f '%u:%g' "$1" 2>/dev/null || stat -c '%u:%g' "$1" 2>/dev/null
}

regular_mode_owner() {
  path=$1
  mode=$2
  test -f "$path" && test ! -L "$path" || return 1
  test "$(mode_of "$path")" = "$mode" || return 1
  test "$(owner_of "$path")" = 0:0
}

validate_release() {
  root=$1
  test -d "$root" && test ! -L "$root" || return 1
  test "$(owner_of "$root")" = 0:0 || return 1
  test "$(mode_of "$root")" = 700 || return 1
  test "$(find "$root" -mindepth 1 | wc -l)" -eq 17 || return 1
  verify_manifest "$root" "$root/manifest.public.sha256" "$root/public-paths" || return 1
  verify_manifest "$root" "$root/manifest.certificates.sha256" "$root/certificate-paths" || return 1
  verify_manifest "$root" "$root/manifest.private.sha256" "$root/private-paths" || return 1
  for specification in \
    'bootstrap.sh 700' 'lib.sh 600' 'zrepl.yml 600' \
    'rc.conf.d-zrepl 644' 'managed-files 600' 'validation-datasets 600' \
    'bundle.meta 600' 'public-paths 600' 'certificate-paths 600' 'private-paths 600' \
    'manifest.public.sha256 600' 'manifest.certificates.sha256 600' 'manifest.private.sha256 600' \
    'tls/mali.crt 644' 'tls/rsyncnet.crt 644' 'tls/rsyncnet.key 600'; do
    # Intentional split of fixed, internal pairs.
    # shellcheck disable=SC2086
    set -- $specification
    regular_mode_owner "$root/$1" "$2" || return 1
  done
}

validate_release_private_copy() {
  root=$1
  test -d "$root" && test ! -L "$root" || return 1
  test "$(owner_of "$root")" = 0:0 && test "$(mode_of "$root")" = 700 || return 1
  regular_mode_owner "$root/private-paths" 600 || return 1
  test "$(cat "$root/private-paths")" = tls/rsyncnet.key || return 1
  regular_mode_owner "$root/manifest.private.sha256" 600 || return 1
  regular_mode_owner "$root/tls/rsyncnet.key" 600 || return 1
  verify_manifest "$root" "$root/manifest.private.sha256" "$root/private-paths"
}
select_private_key_source() {
  retained=/mnt/local/etc2/rsyncnet.key
  selected=
  if regular_mode_owner "$retained" 600; then
    selected=$retained
  fi
  if test -L "$RECOVERY_ROOT/current"; then
    current_release=$(realpath "$RECOVERY_ROOT/current") || return 1
    case $current_release in "$RECOVERY_ROOT"/releases/*) ;; *) return 1 ;; esac
    current_key=$current_release/tls/rsyncnet.key
    if validate_release_private_copy "$current_release"; then
      if test -n "$selected"; then
        cmp -s "$selected" "$current_key" || return 1
      else
        selected=$current_key
      fi
    fi
  fi
  test -n "$selected" || return 1
  PRIVATE_KEY_SOURCE=$selected
}
test "$#" -eq 1 || usage
BUNDLE_ID=$1
RECOVERY_ROOT=/mnt/local/zrepl-recovery
RECEIVER_TLS_SOURCE=/mnt/local/etc2

valid_bundle_id "$BUNDLE_ID" || usage
expected_prefix=$("$SOURCE_DIR/bundle-id.sh" initial)
expected_prefix=${expected_prefix%-initial}
case $BUNDLE_ID in "$expected_prefix"-*) ;; *) exit 65 ;; esac

test "$(id -u)" -eq 0 || exit 77
tab=$(printf '\t')
test "$(zfs list -H -o name,mountpoint,mounted data1/local 2>/dev/null)" = "data1/local${tab}/mnt/local${tab}yes" || exit 69

test -d "$RECEIVER_TLS_SOURCE" && test ! -L "$RECEIVER_TLS_SOURCE" || exit 66
test "$(owner_of "$RECEIVER_TLS_SOURCE")" = 0:0 || exit 77
regular_mode_owner "$RECEIVER_TLS_SOURCE/mali.crt" 644 || exit 77
regular_mode_owner "$RECEIVER_TLS_SOURCE/rsyncnet.crt" 644 || exit 77
select_private_key_source || exit 77

install -d -o root -g wheel -m 0700 \
  "$RECOVERY_ROOT" "$RECOVERY_ROOT/releases" "$RECOVERY_ROOT/backups" "$RECOVERY_ROOT/state"
staging=$RECOVERY_ROOT/releases/.staging-$BUNDLE_ID.$$
release=$RECOVERY_ROOT/releases/$BUNDLE_ID
current_tmp=$RECOVERY_ROOT/.current-$BUNDLE_ID.$$
test ! -e "$staging" && test ! -L "$staging" || exit 73
install -d -o root -g wheel -m 0700 "$staging" "$staging/tls"
trap 'rm -f "$current_tmp"; rm -rf "$staging"' EXIT HUP INT TERM

for specification in \
  'bootstrap.sh 0700' 'lib.sh 0600' 'zrepl.yml 0600' \
  'rc.conf.d-zrepl 0644' 'managed-files 0600' 'validation-datasets 0600' \
  'public-paths 0600' 'certificate-paths 0600' 'private-paths 0600'; do
  # Intentional split of fixed, internal pairs.
  # shellcheck disable=SC2086
  set -- $specification
  install -o root -g wheel -m "$2" "$SOURCE_DIR/$1" "$staging/$1"
done

sed "s/@BUNDLE_ID@/$BUNDLE_ID/" "$SOURCE_DIR/bundle.meta.in" >"$staging/bundle.meta"
chown root:wheel "$staging/bundle.meta"
chmod 0600 "$staging/bundle.meta"
install -o root -g wheel -m 0644 "$RECEIVER_TLS_SOURCE/mali.crt" "$staging/tls/mali.crt"
install -o root -g wheel -m 0644 "$RECEIVER_TLS_SOURCE/rsyncnet.crt" "$staging/tls/rsyncnet.crt"
install -o root -g wheel -m 0600 "$PRIVATE_KEY_SOURCE" "$staging/tls/rsyncnet.key"

write_manifest "$staging" "$staging/public-paths" "$staging/manifest.public.sha256"
write_manifest "$staging" "$staging/certificate-paths" "$staging/manifest.certificates.sha256"
write_manifest "$staging" "$staging/private-paths" "$staging/manifest.private.sha256"
chown root:wheel "$staging/manifest.public.sha256" "$staging/manifest.certificates.sha256" "$staging/manifest.private.sha256"
chmod 0600 "$staging/manifest.public.sha256" "$staging/manifest.certificates.sha256" "$staging/manifest.private.sha256"
validate_release "$staging" || exit 73

if test -e "$release" || test -L "$release"; then
  validate_release "$release" || exit 73
  cmp -s "$staging/manifest.public.sha256" "$release/manifest.public.sha256" || exit 73
  cmp -s "$staging/manifest.certificates.sha256" "$release/manifest.certificates.sha256" || exit 73
  cmp -s "$staging/manifest.private.sha256" "$release/manifest.private.sha256" || exit 73
  rm -rf "$staging"
else
  mv "$staging" "$release"
  validate_release "$release" || exit 73
fi

ln -s "releases/$BUNDLE_ID" "$current_tmp"
mv -fh "$current_tmp" "$RECOVERY_ROOT/current"

test "$(realpath "$RECOVERY_ROOT/current")" = "$release"

trap - EXIT HUP INT TERM
printf 'bundle=%s public_manifest=%s current=%s\n' \
  "$BUNDLE_ID" "$(sha256_file "$release/manifest.public.sha256")" "$(realpath "$RECOVERY_ROOT/current")"
