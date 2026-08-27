#!/bin/sh

# Shared, side-effect-free helpers for the receiver recovery bundle.

sha256_file() {
  if command -v sha256 >/dev/null 2>&1; then
    sha256 -q "$1"
  else
    sha256sum "$1" | awk '{print $1}'
  fi
}

valid_bundle_id() {
  case ${1-} in
    '' | *[!A-Za-z0-9._-]*) return 1 ;;
    *) return 0 ;;
  esac
}

valid_result_token() {
  case ${1-} in
    none | healthy | active | repaired | lock-busy | trust | mount | bundle | key | system | pool | capacity | network | package-plan | package-install | live-mismatch | config | service | control | job | snapshot | replication | rollback | internal) return 0 ;;
    *) return 1 ;;
  esac
}

directory_mode_safe() {
  mode=$1
  case $mode in '' | *[!0-7]*) return 1 ;; esac
  test $(((mode / 10 % 10) & 2)) -eq 0 || return 1
  test $(((mode % 10) & 2)) -eq 0
}
valid_state() {
  case ${1-} in
    ABSENT | PARTIAL | HEALTHY | CATCHING-UP | ERROR) return 0 ;;
    *) return 1 ;;
  esac
}

emit_result() {
  initial=$1
  final=$2
  changed=$3
  bundle=$4
  reason=$5
  validation=$6

  valid_state "$initial" || return 1
  valid_state "$final" || return 1
  case $changed in 0 | 1) ;; *) return 1 ;; esac
  valid_bundle_id "$bundle" || return 1
  valid_result_token "$reason" || return 1
  case $validation in pass | pending | fail) ;; *) return 1 ;; esac

  printf 'ZREPL_BOOTSTRAP_V1 initial=%s final=%s changed=%s bundle=%s reason=%s validation=%s\n' \
    "$initial" "$final" "$changed" "$bundle" "$reason" "$validation"
}

status_evidence() {
  status_file=$1

  test -s "$status_file" || {
    printf '%s\n' INVALID
    return
  }

  if grep -Eq 'PLANNING-ERROR|REPLICATION-ERROR|STEP-ERROR|ConflictDiverged|no common ancestor' "$status_file"; then
    printf '%s\n' CONTINUITY-ERROR
  elif grep -Eq '(^|[^A-Z])(PLANNING|STEPPING|SENDING|RECEIVING)([^A-Z]|$)' "$status_file"; then
    printf '%s\n' ACTIVE
  elif grep -Eq '(^|[[:space:]])pull_mali([[:space:]]|$)' "$status_file"; then
    printf '%s\n' IDLE
  else
    printf '%s\n' INVALID
  fi
}

status_state() {
  case $(status_evidence "$1") in
    CONTINUITY-ERROR | INVALID) printf '%s\n' ERROR ;;
    ACTIVE) printf '%s\n' CATCHING-UP ;;
    IDLE) printf '%s\n' HEALTHY ;;
  esac
}

post_start_action() {
  case ${1-} in
    ACTIVE) printf '%s\n' PRESERVE-CATCHING-UP ;;
    CONTINUITY-ERROR) printf '%s\n' PRESERVE-ERROR ;;
    IDLE | INVALID) printf '%s\n' ROLLBACK ;;
    *) return 1 ;;
  esac
}
classify_state() {
  process_present=$1
  control_present=$2
  integration_valid=$3
  any_installation=$4
  status_file=$5

  if test "$process_present" = 1 || test "$control_present" = 1; then
    if test "$process_present" != 1 || test "$control_present" != 1 || test "$integration_valid" != 1; then
      printf '%s\n' ERROR
    else
      status_state "$status_file"
    fi
  elif test "$any_installation" = 0; then
    printf '%s\n' ABSENT
  else
    printf '%s\n' PARTIAL
  fi
}

config_value() {
  config=$1
  key=$2
  awk -v key="$key" '$1 == key ":" { gsub(/"/, "", $2); print $2; count++ } END { if (count != 1) exit 1 }' "$config"
}

config_endpoint() {
  config=$1
  endpoint=$(config_value "$config" address) || return 1
  host=${endpoint%:*}
  port=${endpoint##*:}
  case $host in '' | *[!0-9.]*) return 1 ;; esac
  case $port in '' | *[!0-9]*) return 1 ;; esac
  printf '%s %s\n' "$host" "$port"
}

validate_dataset_list() {
  datasets=$1
  count=0
  while IFS= read -r dataset; do
    case $dataset in
      data1/replication/mali/*) ;;
      *) return 1 ;;
    esac
    case $dataset in
      *[!A-Za-z0-9_./:%-]* | *'..'* | *'//'*) return 1 ;;
    esac
    observed=$(zfs list -H -o name "$dataset" 2>/dev/null) || return 1
    test "$observed" = "$dataset" || return 1
    count=$((count + 1))
  done <"$datasets"
  test "$count" -gt 0
}

snapshot_marker() {
  dataset=$1
  snapshot=$2
  guid=$3
  creation=$4
  case $snapshot in "$dataset"@*) ;; *) return 1 ;; esac
  leaf=${snapshot#"$dataset"@}
  test -n "$leaf" && test "${#leaf}" -le 255 || return 1
  case $leaf in *[!A-Za-z0-9_.:%+-]*) return 1 ;; esac
  case $guid in '' | *[!0-9]*) return 1 ;; esac
  case $creation in '' | *[!0-9]*) return 1 ;; esac
  printf 'ZREPL_SNAPSHOT_V1 dataset=%s snapshot=%s guid=%s creation=%s\n' \
    "$dataset" "$snapshot" "$guid" "$creation"
}

capture_snapshot_markers() {
  datasets=$1
  output=$2
  temporary=$output.$$
  : >"$temporary" || return 1
  while IFS= read -r dataset; do
    raw=$output.raw.$$
    zfs list -H -p -t snapshot -d 1 -s creation -o name,guid,creation "$dataset" >"$raw" 2>/dev/null || {
      rm -f "$temporary" "$raw"
      return 1
    }
    marker=$(awk 'END {print $1 "|" $2 "|" $3}' "$raw") || {
      rm -f "$temporary" "$raw"
      return 1
    }
    rm -f "$raw"
    snapshot=${marker%%|*}
    remainder=${marker#*|}
    guid=${remainder%%|*}
    creation=${remainder#*|}
    if test "$snapshot" = "$marker" || test "$guid" = "$remainder" || ! snapshot_marker "$dataset" "$snapshot" "$guid" "$creation" >>"$temporary"; then
      rm -f "$temporary"
      return 1
    fi
  done <"$datasets"
  mv -f "$temporary" "$output"
}
validate_pkg_check_result() {
  output=$1
  result=$2
  expected_version=$3
  if test "$result" -eq 0; then
    test ! -s "$output"
    return
  fi
  expected=$(mktemp "${TMPDIR:-/tmp}/zrepl-pkg-check-expected.XXXXXX") || return 1
  printf 'zrepl-%s: checksum mismatch for /usr/local/etc/zrepl/zrepl.yml\n' "$expected_version" >"$expected"
  cmp -s "$expected" "$output"
  matched=$?
  rm -f "$expected"
  return "$matched"
}

manifest_paths_safe() {
  manifest=$1
  test -f "$manifest" || return 1

  awk '
    NF != 2 { exit 1 }
    $1 !~ /^[0-9a-f]{64}$/ { exit 1 }
    $2 !~ /^[A-Za-z0-9][A-Za-z0-9._\/-]*$/ { exit 1 }
    $2 ~ /(^|\/)\.\.?(\/|$)/ { exit 1 }
    $2 ~ /^\// { exit 1 }
    { seen[$2]++; if (seen[$2] > 1) exit 1 }
    END { if (NR == 0) exit 1 }
  ' "$manifest"
}

verify_manifest() {
  root=$1
  manifest=$2
  expected_paths=$3

  manifest_paths_safe "$manifest" || return 1
  actual_paths=$(mktemp "${TMPDIR:-/tmp}/zrepl-manifest.actual.XXXXXX") || return 1
  awk '{print $2}' "$manifest" | LC_ALL=C sort >"$actual_paths"
  if ! LC_ALL=C sort "$expected_paths" | cmp -s - "$actual_paths"; then
    rm -f "$actual_paths"
    return 1
  fi

  while read -r expected relative; do
    path=$root/$relative
    if ! test -f "$path" || test -L "$path"; then
      rm -f "$actual_paths"
      return 1
    fi
    actual=$(sha256_file "$path") || {
      rm -f "$actual_paths"
      return 1
    }
    if test "$actual" != "$expected"; then
      rm -f "$actual_paths"
      return 1
    fi
  done <"$manifest"

  rm -f "$actual_paths"
}

write_manifest() {
  root=$1
  paths_file=$2
  destination=$3
  temporary=$destination.tmp.$$

  : >"$temporary" || return 1
  while read -r relative; do
    case $relative in '' | /* | . | .. | ../* | */../* | */..)
      rm -f "$temporary"
      return 1
      ;;
    esac
    path=$root/$relative
    test -f "$path" && test ! -L "$path" || {
      rm -f "$temporary"
      return 1
    }
    printf '%s  %s\n' "$(sha256_file "$path")" "$relative" >>"$temporary" || {
      rm -f "$temporary"
      return 1
    }
  done <"$paths_file"
  chmod 0600 "$temporary" && mv -f "$temporary" "$destination"
}

package_plan_actions() {
  awk '
    /^(New packages to be INSTALLED|Installed packages to be UPGRADED|Installed packages to be REINSTALLED|Installed packages to be DOWNGRADED|Installed packages to be REMOVED):/ {
      section=$0
      sub(/:.*/, "", section)
      next
    }
    /^[A-Z][A-Za-z -]+:/ { section=""; next }
    section != "" && /^[[:space:]]+[A-Za-z0-9][A-Za-z0-9_.+-]*:/ {
      line=$0
      sub(/^[[:space:]]+/, "", line)
      name=line
      sub(/:.*/, "", name)
      version=line
      sub(/^[^:]+:[[:space:]]*/, "", version)
      if (version ~ /[[:space:]]->[[:space:]]/) sub(/^.*[[:space:]]->[[:space:]]*/, "", version)
      sub(/[[:space:]].*/, "", version)
      print section "|" name "|" version
    }
  ' "$1"
}

package_plan_zrepl_version() {
  package_plan_actions "$1" | awk -F '|' '
    $2 == "zrepl" {
      count++
      version=$3
    }
    END {
      if (count != 1 || version !~ /^[A-Za-z0-9][A-Za-z0-9._,+-]*$/) exit 1
      print version
    }
  '
}

package_manifest_version() {
  awk '
    /^[[:space:]]*version[[:space:]]*[:=]/ {
      line=$0
      sub(/^[[:space:]]*version[[:space:]]*[:=][[:space:]]*/, "", line)
      sub(/[,;][[:space:]]*$/, "", line)
      gsub(/^"|"$/, "", line)
      count++
      version=line
    }
    END {
      if (count != 1 || version !~ /^[A-Za-z0-9][A-Za-z0-9._,+-]*$/) exit 1
      print version
    }
  ' "$1"
}

package_candidate_matches_plan() {
  test "$(package_plan_zrepl_version "$1")" = "$(package_manifest_version "$2")"
}

validate_package_plan() {
  plan=$1
  actions=$(mktemp "${TMPDIR:-/tmp}/zrepl-package-plan.XXXXXX") || return 1
  package_plan_actions "$plan" >"$actions"
  if ! test -s "$actions"; then
    rm -f "$actions"
    return 1
  fi

  if ! awk -F '|' '
    $1 ~ /DOWNGRADED|REMOVED/ { exit 1 }
    $2 != "pkg" && $2 != "zrepl" { exit 1 }
    $2 == "zrepl" {
      zrepl++
      if ($3 !~ /^[A-Za-z0-9][A-Za-z0-9._,+-]*$/) exit 1
    }
    END { if (zrepl != 1) exit 1 }
  ' "$actions"; then
    rm -f "$actions"
    return 1
  fi

  rm -f "$actions"
}

package_plan_has_pkg_upgrade() {
  package_plan_actions "$1" | awk -F '|' '$1 ~ /UPGRADED/ && $2 == "pkg" { found=1 } END { exit !found }'
}

validate_pkg_self_plan() {
  actions=$(mktemp "${TMPDIR:-/tmp}/zrepl-pkg-self-plan.XXXXXX") || return 1
  package_plan_actions "$1" >"$actions"
  if ! awk -F '|' '
    $1 ~ /DOWNGRADED|REMOVED/ { exit 1 }
    $1 !~ /UPGRADED/ || $2 != "pkg" { exit 1 }
    { count++ }
    END { if (count != 1) exit 1 }
  ' "$actions"; then
    rm -f "$actions"
    return 1
  fi
  rm -f "$actions"
}
package_plan_required_bytes() {
  awk '
    /The process will require/ {
      value=$(NF-3)
      unit=$(NF-2)
      multiplier=0
      if (unit == "B") multiplier=1
      else if (unit == "KiB") multiplier=1024
      else if (unit == "MiB") multiplier=1024*1024
      else if (unit == "GiB") multiplier=1024*1024*1024
      if (value !~ /^[0-9]+([.][0-9]+)?$/ || multiplier == 0) exit 1
      printf "%.0f\n", value*multiplier
      found=1
    }
    END { if (!found) exit 1 }
  ' "$1"
}

validate_package_manifest() {
  manifest=$1
  expected_origin=$2
  expected_arch=$3
  grep -Eq '^[[:space:]]*name[[:space:]]*[:=][[:space:]]*"?zrepl"?[,;]?$' "$manifest" || return 1
  grep -Eq '^[[:space:]]*version[[:space:]]*[:=][[:space:]]*"?[A-Za-z0-9][A-Za-z0-9._,+-]*"?[,;]?$' "$manifest" || return 1
  grep -Eq "^[[:space:]]*origin[[:space:]]*[:=][[:space:]]*\"?$expected_origin\"?[,;]?$" "$manifest" || return 1
  grep -Eq "^[[:space:]]*arch[[:space:]]*[:=][[:space:]]*\"?$expected_arch\"?[,;]?$" "$manifest" || return 1
  if grep -Eq '^[[:space:]]*(scripts|lua_scripts)[[:space:]]*[:={]' "$manifest"; then
    return 1
  fi
}
