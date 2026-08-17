#!/bin/sh
set -u
umask 077
PATH=/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin
export PATH

RECOVERY_ROOT=/mnt/local/zrepl-recovery
SELF=$(realpath "$0" 2>/dev/null || printf '%s' "$0")
RELEASE_ROOT=$(dirname "$SELF")
LIB=$RELEASE_ROOT/lib.sh
BUNDLE_ID=unknown
INITIAL=ERROR
CHANGED=0
RESULT_WRITTEN=0
MARKERS_READY=0
SNAPSHOT_MARKERS=/tmp/zrepl-bootstrap-snapshots.$$

if test -f "$LIB" && test ! -L "$LIB"; then
  # shellcheck source=/dev/null
  . "$LIB"
else
  printf '%s\n' 'ZREPL_BOOTSTRAP_V1 initial=ERROR final=ERROR changed=0 bundle=unknown reason=bundle validation=fail'
  exit 70
fi

finish() {
  final=$1
  reason=$2
  validation=$3
  code=$4
  if test "$RESULT_WRITTEN" = 0; then
    if test "$MARKERS_READY" = 1; then
      if capture_snapshot_markers "$RELEASE_ROOT/validation-datasets" "$SNAPSHOT_MARKERS"; then
        cat "$SNAPSHOT_MARKERS"
      else
        final=ERROR
        reason=snapshot
        validation=fail
        code=69
      fi
      rm -f "$SNAPSHOT_MARKERS" "$SNAPSHOT_MARKERS.raw.$$"
    fi
    emit_result "$INITIAL" "$final" "$CHANGED" "$BUNDLE_ID" "$reason" "$validation" || :
    RESULT_WRITTEN=1
  fi
  exit "$code"
}

meta_value() {
  key=$1
  awk -F '=' -v key="$key" '$1 == key { sub(/^[^=]*=/, ""); print; found=1 } END { if (!found) exit 1 }' "$RELEASE_ROOT/bundle.meta"
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

managed_live_valid() {
  expected_package_valid || return 1
  while IFS='|' read -r source destination mode; do
    regular_mode_owner "$destination" "$mode" || return 1
    cmp -s "$RELEASE_ROOT/$source" "$destination" || return 1
  done <"$RELEASE_ROOT/managed-files"
  /usr/local/bin/zrepl --config /usr/local/etc/zrepl/zrepl.yml configcheck >/dev/null 2>&1
}

any_installation_present() {
  pkg info -e 'zrepl-*' >/dev/null 2>&1 && return 0
  for path in /usr/local/bin/zrepl /usr/local/etc/rc.d/zrepl; do
    test -e "$path" && return 0
  done
  while IFS='|' read -r _source destination _mode; do
    test -e "$destination" && return 0
  done <"$RELEASE_ROOT/managed-files"
  return 1
}

capture_status() {
  status_file=$1
  : >"$status_file"
  /usr/local/bin/zrepl status --mode dump >"$status_file" 2>/dev/null
}

verify_release() {
  expected_public=$RELEASE_ROOT/public-paths
  expected_certificates=$RELEASE_ROOT/certificate-paths
  expected_private=$RELEASE_ROOT/private-paths
  verify_manifest "$RELEASE_ROOT" "$RELEASE_ROOT/manifest.public.sha256" "$expected_public" || return 1
  verify_manifest "$RELEASE_ROOT" "$RELEASE_ROOT/manifest.certificates.sha256" "$expected_certificates" || return 1
  verify_manifest "$RELEASE_ROOT" "$RELEASE_ROOT/manifest.private.sha256" "$expected_private" || return 1

  test "$(meta_value package_name)" = zrepl || return 1
  test "$(meta_value package_origin)" = sysutils/zrepl || return 1
  test "$(meta_value package_arch)" = FreeBSD:14:amd64 || return 1
  test "$(meta_value pruning_mode)" = keep-all || return 1
  grep -Fxq "zrepl.yml|$(meta_value live_config)|600" "$RELEASE_ROOT/managed-files" || return 1
  grep -Fxq "rc.conf.d-zrepl|$(meta_value live_rc)|644" "$RELEASE_ROOT/managed-files" || return 1
  config_endpoint "$RELEASE_ROOT/zrepl.yml" >/dev/null || return 1
  grep -Fxq "tls/mali.crt|$(config_value "$RELEASE_ROOT/zrepl.yml" ca)|644" "$RELEASE_ROOT/managed-files" || return 1
  grep -Fxq "tls/rsyncnet.crt|$(config_value "$RELEASE_ROOT/zrepl.yml" cert)|644" "$RELEASE_ROOT/managed-files" || return 1
  grep -Fxq "tls/rsyncnet.key|$(config_value "$RELEASE_ROOT/zrepl.yml" key)|600" "$RELEASE_ROOT/managed-files" || return 1
  test "$(config_value "$RELEASE_ROOT/zrepl.yml" root_fs)" = "$(meta_value receiver_root)" || return 1
  for specification in \
    'bootstrap.sh 700' 'lib.sh 600' 'zrepl.yml 600' \
    'rc.conf.d-zrepl 644' 'managed-files 600' 'validation-datasets 600' \
    'bundle.meta 600' 'public-paths 600' 'certificate-paths 600' 'private-paths 600' \
    'manifest.public.sha256 600' 'manifest.certificates.sha256 600' 'manifest.private.sha256 600' \
    'tls/mali.crt 644' 'tls/rsyncnet.crt 644' 'tls/rsyncnet.key 600'; do
    # Intentional split of fixed, internal pairs.
    # shellcheck disable=SC2086
    set -- $specification
    regular_mode_owner "$RELEASE_ROOT/$1" "$2" || return 1
  done

  test "$(find "$RELEASE_ROOT" -mindepth 1 | wc -l)" -eq 17 || return 1
  case $RELEASE_ROOT in "$RECOVERY_ROOT"/releases/*) ;; *) return 1 ;; esac
  test "$(owner_of "$RELEASE_ROOT")" = 0:0 || return 1
  mode=$(mode_of "$RELEASE_ROOT") || return 1
  test $((mode % 100)) -eq 0 || return 1
}

verify_mount_and_system() {
  test "$(zfs list -H -o name,mountpoint,mounted data1/local 2>/dev/null)" = "data1/local${TAB}/mnt/local${TAB}yes" || return 1
  test "$(hostname -f 2>/dev/null)" = "$(meta_value receiver_hostname)" || return 1
  test "$(uname -m)" = "$(meta_value freebsd_arch)" || return 1
  release=$(freebsd-version -u 2>/dev/null) || return 1
  test "$(meta_value freebsd_release_pattern)" = '14.*' || return 1
  case $release in 14.*) ;; *) return 1 ;; esac
}

verify_pool_capacity() {
  pool=$(meta_value pool_name) || return 1
  receiver_root=$(meta_value receiver_root) || return 1
  pool_line=$(zpool list -Hp -o name,size,free,health "$pool" 2>/dev/null) || return 1
  # Intentional split of four machine-formatted fields.
  # shellcheck disable=SC2086
  set -- $pool_line
  test "$1" = "$pool" && test "$4" = ONLINE || return 1
  minimum_bytes=$(meta_value minimum_pool_free_bytes) || return 1
  minimum_percent=$(meta_value minimum_pool_free_percent) || return 1
  test "$3" -ge "$minimum_bytes" || return 1
  test $((100 * $3 / $2)) -ge "$minimum_percent" || return 1
  zfs list -H -o name "$receiver_root" >/dev/null 2>&1 || return 1
  test "$(df -Pk / | awk 'NR == 2 {print $4}')" -gt 131072 || return 1
  test "$(df -Pk /mnt/local | awk 'NR == 2 {print $4}')" -gt 16384
}

verify_network() {
  endpoint=$(config_endpoint "$RELEASE_ROOT/zrepl.yml") || return 1
  # Intentional split of validated host and port.
  # shellcheck disable=SC2086
  set -- $endpoint
  nc -z -w 10 "$1" "$2" >/dev/null 2>&1
}

size_of() {
  stat -f '%z' "$1" 2>/dev/null || stat -c '%s' "$1" 2>/dev/null
}

managed_backup_required_bytes() {
  total=1048576
  while IFS='|' read -r _source path _mode; do
    if test -e "$path" || test -L "$path"; then
      test -f "$path" && test ! -L "$path" || return 1
      size=$(size_of "$path") || return 1
      total=$((total + size))
    fi
  done <"$RELEASE_ROOT/managed-files"
  printf '%s\n' "$total"
}

planned_package_required_bytes() {
  force=$1
  plan=$(mktemp /tmp/zrepl-capacity-plan.XXXXXX) || return 1
  if test "$force" = 1; then
    pkg install -nf zrepl >"$plan" 2>&1 || {
      rm -f "$plan"
      return 1
    }
  else
    pkg install -n zrepl >"$plan" 2>&1 || {
      rm -f "$plan"
      return 1
    }
  fi
  validate_package_plan "$plan" "$(meta_value package_version)" || {
    rm -f "$plan"
    return 1
  }
  required=$(package_plan_required_bytes "$plan") || {
    rm -f "$plan"
    return 1
  }
  rm -f "$plan"
  printf '%s\n' "$required"
}

verify_repair_capacity() {
  package_bytes=$1
  backup_bytes=$2
  root_available_kib=$(df -Pk / | awk 'NR == 2 {print $4}') || return 1
  persistent_available_kib=$(df -Pk /mnt/local | awk 'NR == 2 {print $4}') || return 1
  test $((root_available_kib * 1024)) -gt $((package_bytes + 134217728)) || return 1
  test $((persistent_available_kib * 1024)) -gt $((backup_bytes + 16777216))
}
safe_live_directory() {
  directory=$1
  test -d "$directory" && test ! -L "$directory" || return 1
  test "$(owner_of "$directory")" = 0:0 || return 1
  directory_mode=$(mode_of "$directory") || return 1
  directory_mode_safe "$directory_mode"
}

safe_directory_chain() {
  directory=$1
  case $directory in /*) ;; *) return 1 ;; esac
  safe_live_directory / || return 1
  relative=${directory#/}
  current=
  while test -n "$relative"; do
    case $relative in
      */*)
        component=${relative%%/*}
        relative=${relative#*/}
        ;;
      *)
        component=$relative
        relative=
        ;;
    esac
    test -n "$component" || return 1
    current=$current/$component
    safe_live_directory "$current" || return 1
  done
}
validate_live_parent() {
  directory=$(dirname "$1")
  if test -e "$directory" || test -L "$directory"; then
    safe_directory_chain "$directory"
  else
    safe_directory_chain "$(dirname "$directory")"
  fi
}

prepare_live_parent() {
  directory=$(dirname "$1")
  if test -e "$directory" || test -L "$directory"; then
    safe_directory_chain "$directory"
  else
    safe_directory_chain "$(dirname "$directory")" || return 1
    install -d -o root -g wheel -m 0755 "$directory" || return 1
    safe_directory_chain "$directory" || return 1
    printf '%s\n' "$directory" >>"$BACKUP/created-directories" || return 1
  fi
}

backup_live_files() {
  timestamp=$(date -u +%Y%m%dT%H%M%SZ) || return 1
  BACKUP=$RECOVERY_ROOT/backups/$timestamp-$BUNDLE_ID
  test ! -e "$BACKUP" || return 1
  install -d -o root -g wheel -m 0700 "$BACKUP" || return 1
  install -o root -g wheel -m 0600 "$RELEASE_ROOT/managed-files" "$BACKUP/managed-files" || return 1
  : >"$BACKUP/created-directories" || return 1
  chmod 0600 "$BACKUP/created-directories" || return 1
  index=0
  while IFS='|' read -r _source path _mode; do
    validate_live_parent "$path" || return 1
    index=$((index + 1))
    name=$(printf '%02d' "$index")
    if test -e "$path" || test -L "$path"; then
      test -f "$path" && test ! -L "$path" || return 1
      cp -p "$path" "$BACKUP/$name.file" || return 1
      printf '%s|%s\n' "$(owner_of "$path")" "$(mode_of "$path")" >"$BACKUP/$name.metadata" || return 1
    else
      : >"$BACKUP/$name.absent" || return 1
    fi
    printf '%s\n' "$path" >"$BACKUP/$name.path" || return 1
  done <"$BACKUP/managed-files"
  chmod -R go-rwx "$BACKUP"
}

restore_live_files() {
  index=0
  while IFS='|' read -r _source path _mode; do
    index=$((index + 1))
    name=$(printf '%02d' "$index")
    test "$(cat "$BACKUP/$name.path")" = "$path" || return 1
    prepare_live_parent "$path" || return 1
    if test -f "$BACKUP/$name.file"; then
      metadata=$(cat "$BACKUP/$name.metadata") || return 1
      owner=${metadata%|*}
      restore_mode=${metadata##*|}
      uid=${owner%:*}
      gid=${owner##*:}
      install -o "$uid" -g "$gid" -m "$restore_mode" "$BACKUP/$name.file" "$path" || return 1
    elif test -f "$BACKUP/$name.absent"; then
      if test -e "$path" || test -L "$path"; then
        test -f "$path" && test ! -L "$path" || return 1
      fi
      rm -f "$path" || return 1
    else
      return 1
    fi
  done <"$BACKUP/managed-files"
  reverse_directories=$BACKUP/created-directories.reverse
  awk '{ paths[NR]=$0 } END { for (i=NR; i>0; i--) print paths[i] }' "$BACKUP/created-directories" >"$reverse_directories" || return 1
  while IFS= read -r directory; do
    safe_directory_chain "$directory" || return 1
    rmdir "$directory" || return 1
  done <"$reverse_directories"
  rm -f "$reverse_directories"
}

inspect_package_scripts() {
  metadata_dir=$1
  expected_version=$(meta_value package_version) || return 1
  expected_origin=$(meta_value package_origin) || return 1
  expected_arch=$(meta_value package_arch) || return 1
  install -d -o root -g wheel -m 0700 "$metadata_dir" || return 1
  pkg fetch -y -o "$metadata_dir" zrepl >"$metadata_dir/fetch.log" 2>&1 || return 1
  archive_count=$(find "$metadata_dir" -maxdepth 1 -type f \( -name '*.pkg' -o -name '*.txz' \) | wc -l)
  test "$archive_count" -eq 1 || return 1
  archive=$(find "$metadata_dir" -maxdepth 1 -type f \( -name '*.pkg' -o -name '*.txz' \))
  pkg info -F "$archive" -R --raw-format ucl >"$metadata_dir/manifest.ucl" 2>&1 || return 1
  validate_package_manifest "$metadata_dir/manifest.ucl" "$expected_version" "$expected_origin" "$expected_arch"
}

inspect_package_candidate() {
  evidence_name=$1
  metadata_dir=$(mktemp -d /tmp/zrepl-package-inspect.XXXXXX) || return 1
  inspect_package_scripts "$metadata_dir"
  result=$?
  if test "$result" -eq 0; then
    install -o root -g wheel -m 0600 "$metadata_dir/manifest.ucl" "$BACKUP/$evidence_name.manifest.ucl" || result=1
  fi
  rm -rf "$metadata_dir"
  return "$result"
}

package_payload_valid() {
  expected_version=$(meta_value package_version) || return 1
  check_output=$(mktemp /tmp/zrepl-pkg-check.XXXXXX) || return 1
  pkg check -sq zrepl >/dev/null 2>"$check_output"
  check_result=$?
  validate_pkg_check_result "$check_output" "$check_result" "$expected_version"
  valid=$?
  rm -f "$check_output"
  return "$valid"
}

expected_package_valid() {
  expected_version=$(meta_value package_version) || return 1
  expected_origin=$(meta_value package_origin) || return 1
  expected_arch=$(meta_value package_arch) || return 1
  pkg info -e "zrepl-$expected_version" >/dev/null 2>&1 || return 1
  test "$(pkg query '%n|%v|%o|%q' zrepl 2>/dev/null)" = "zrepl|$expected_version|$expected_origin|$expected_arch" || return 1
  test "$(pkg which -q /usr/local/bin/zrepl 2>/dev/null)" = "zrepl-$expected_version" || return 1
  test "$(pkg which -q /usr/local/etc/rc.d/zrepl 2>/dev/null)" = "zrepl-$expected_version" || return 1
  package_payload_valid || return 1
  test -x /usr/local/bin/zrepl && test -f /usr/local/bin/zrepl && test ! -L /usr/local/bin/zrepl || return 1
  test -x /usr/local/etc/rc.d/zrepl && test -f /usr/local/etc/rc.d/zrepl && test ! -L /usr/local/etc/rc.d/zrepl
}

install_expected_package() {
  force=$1
  expected_version=$(meta_value package_version) || return 1
  plan=$BACKUP/pkg-install.dry-run
  if test "$force" = 1; then
    pkg install -nf zrepl >"$plan" 2>&1 || return 1
  else
    pkg install -n zrepl >"$plan" 2>&1 || return 1
  fi
  validate_package_plan "$plan" "$expected_version" || return 1
  inspect_package_candidate pkg-candidate || return 1

  if package_plan_has_pkg_upgrade "$plan"; then
    pkg install -n pkg >"$BACKUP/pkg-self-upgrade.dry-run" 2>&1 || return 1
    validate_pkg_self_plan "$BACKUP/pkg-self-upgrade.dry-run" || return 1
    pkg install -y pkg >"$BACKUP/pkg-self-upgrade.log" 2>&1 || return 1
    if test "$force" = 1; then
      pkg install -nf zrepl >"$plan.after-pkg" 2>&1 || return 1
    else
      pkg install -n zrepl >"$plan.after-pkg" 2>&1 || return 1
    fi
    validate_package_plan "$plan.after-pkg" "$expected_version" || return 1
    inspect_package_candidate pkg-candidate-after-pkg || return 1
  fi

  if test "$force" = 1; then
    pkg install -yf zrepl >"$BACKUP/pkg-install.log" 2>&1 || return 1
  else
    pkg install -y zrepl >"$BACKUP/pkg-install.log" 2>&1 || return 1
  fi
  expected_package_valid
}

install_live_file() {
  source=$1
  destination=$2
  mode=$3
  directory=$(dirname "$destination")
  if test -e "$destination" || test -L "$destination"; then
    test -f "$destination" && test ! -L "$destination" || return 1
  fi
  prepare_live_parent "$destination" || return 1
  temporary=$directory/.zrepl-recovery.$$
  install -o root -g wheel -m "$mode" "$source" "$temporary" || return 1
  cmp -s "$source" "$temporary" || {
    rm -f "$temporary"
    return 1
  }
  mv -f "$temporary" "$destination"
}

project_live_files() {
  rc_source=
  rc_destination=
  rc_mode=
  while IFS='|' read -r source destination mode; do
    if test "$source" = rc.conf.d-zrepl; then
      rc_source=$source
      rc_destination=$destination
      rc_mode=$mode
    else
      install_live_file "$RELEASE_ROOT/$source" "$destination" "$mode" || return 1
    fi
  done <"$RELEASE_ROOT/managed-files"
  /usr/local/bin/zrepl --config /usr/local/etc/zrepl/zrepl.yml configcheck >/dev/null 2>&1 || return 1
  test -n "$rc_source" && test -n "$rc_destination" && test -n "$rc_mode" || return 1
  install_live_file "$RELEASE_ROOT/$rc_source" "$rc_destination" "$rc_mode"
}

rollback_started_repair() {
  reason=$1
  status_file=$2
  if pgrep -x zrepl >/dev/null 2>&1 || test -S /var/run/zrepl/control; then
    if test -S /var/run/zrepl/control && capture_status "$status_file"; then
      evidence=$(status_evidence "$status_file")
      action=$(post_start_action "$evidence") || finish ERROR internal fail 70
      case $action in
        PRESERVE-CATCHING-UP) finish CATCHING-UP "$reason" fail 69 ;;
        PRESERVE-ERROR) finish ERROR "$reason" fail 69 ;;
        ROLLBACK) ;;
      esac
    fi
    service zrepl stop >/dev/null 2>&1 || finish ERROR rollback fail 70
  fi
  pgrep -x zrepl >/dev/null 2>&1 && finish ERROR rollback fail 70
  test -S /var/run/zrepl/control && finish ERROR rollback fail 70
  restore_live_files || finish ERROR rollback fail 70
  finish ERROR "$reason" fail 69
}
run_locked() {
  status_file=$(mktemp /tmp/zrepl-bootstrap-status.XXXXXX) || finish ERROR internal fail 70
  trap 'rm -f "$status_file"' HUP INT TERM EXIT

  process=0
  control=0
  integration=0
  installation=0
  status_ok=1
  pgrep -x zrepl >/dev/null 2>&1 && process=1
  test -S /var/run/zrepl/control && control=1
  managed_live_valid && integration=1
  any_installation_present && installation=1
  if test "$process" = 1 && test "$control" = 1; then
    capture_status "$status_file" || status_ok=0
  fi
  INITIAL=$(classify_state "$process" "$control" "$integration" "$installation" "$status_file")

  case $INITIAL in
    HEALTHY) finish HEALTHY healthy pass 0 ;;
    CATCHING-UP) finish CATCHING-UP active pending 0 ;;
    ERROR)
      if test "$process" != 1; then
        finish ERROR service fail 69
      elif test "$control" != 1 || test "$status_ok" != 1; then
        finish ERROR control fail 69
      elif test "$integration" != 1; then
        finish ERROR live-mismatch fail 69
      else
        finish ERROR replication fail 69
      fi
      ;;
    ABSENT | PARTIAL) ;;
    *) finish ERROR internal fail 70 ;;
  esac

  package_force=0
  package_needed=0
  if ! expected_package_valid; then
    package_needed=1
    if pkg info -e "zrepl-$(meta_value package_version)" >/dev/null 2>&1; then
      package_force=1
    fi
  fi
  package_bytes=0
  if test "$package_needed" = 1; then
    package_bytes=$(planned_package_required_bytes "$package_force") || finish ERROR package-plan fail 69
  fi
  backup_bytes=$(managed_backup_required_bytes) || finish ERROR capacity fail 69
  verify_repair_capacity "$package_bytes" "$backup_bytes" || finish ERROR capacity fail 69
  backup_live_files || finish ERROR rollback fail 70

  if test "$package_needed" = 1; then
    install_expected_package "$package_force" || finish ERROR package-plan fail 69
  fi

  project_live_files || {
    restore_live_files || finish ERROR rollback fail 70
    finish ERROR config fail 69
  }
  managed_live_valid || {
    restore_live_files || finish ERROR rollback fail 70
    finish ERROR live-mismatch fail 69
  }

  pgrep -x zrepl >/dev/null 2>&1 && finish ERROR service fail 69
  test -S /var/run/zrepl/control && finish ERROR control fail 69
  CHANGED=1
  service zrepl start >/dev/null 2>&1 || rollback_started_repair service "$status_file"

  sleep 2
  service zrepl status >/dev/null 2>&1 || rollback_started_repair service "$status_file"
  test -S /var/run/zrepl/control || rollback_started_repair control "$status_file"
  capture_status "$status_file" || rollback_started_repair control "$status_file"
  evidence=$(status_evidence "$status_file")
  case $evidence in
    CONTINUITY-ERROR) finish ERROR replication fail 69 ;;
    INVALID) rollback_started_repair job "$status_file" ;;
    ACTIVE | IDLE) finish CATCHING-UP repaired pending 0 ;;
    *) rollback_started_repair internal "$status_file" ;;
  esac
}

TAB=$(printf '\t')

if test -n "${SSH_ORIGINAL_COMMAND-}"; then
  finish ERROR trust fail 77
fi

verify_mount_and_system || finish ERROR mount fail 69
verify_release || finish ERROR bundle fail 69
BUNDLE_ID=$(meta_value bundle_id 2>/dev/null || printf unknown)
valid_bundle_id "$BUNDLE_ID" || {
  BUNDLE_ID=unknown
  finish ERROR bundle fail 69
}
verify_pool_capacity || finish ERROR pool fail 69
validate_dataset_list "$RELEASE_ROOT/validation-datasets" || finish ERROR pool fail 69
MARKERS_READY=1
verify_network || finish ERROR network fail 69

if test "${1-}" = --locked; then
  run_locked
fi

install -d -o root -g wheel -m 0700 "$RECOVERY_ROOT/state" || finish ERROR bundle fail 69
lockf -t 0 "$RECOVERY_ROOT/state/bootstrap.lock" /bin/sh "$SELF" --locked 2>/dev/null
code=$?
if test "$code" -eq 75; then
  finish ERROR lock-busy pending 75
fi
exit "$code"
