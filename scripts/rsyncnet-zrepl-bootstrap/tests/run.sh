#!/bin/sh
set -eu
umask 077

TEST_DIR=$(CDPATH='' cd -- "$(dirname "$0")" && pwd)
SOURCE_DIR=$(dirname "$TEST_DIR")
FIXTURES=$TEST_DIR/fixtures
# shellcheck source=/dev/null
. "$SOURCE_DIR/lib.sh"

passed=0
fail() {
  printf 'not ok - %s\n' "$1" >&2
  exit 1
}
check_eq() {
  description=$1
  expected=$2
  actual=$3
  test "$actual" = "$expected" || fail "$description: expected <$expected>, got <$actual>"
  passed=$((passed + 1))
}
check_true() {
  description=$1
  shift
  "$@" || fail "$description"
  passed=$((passed + 1))
}
check_false() {
  description=$1
  shift
  if "$@"; then fail "$description"; fi
  passed=$((passed + 1))
}

check_eq 'healthy status' HEALTHY "$(status_state "$FIXTURES/status-healthy.txt")"
check_eq 'active status' CATCHING-UP "$(status_state "$FIXTURES/status-active.txt")"
check_eq 'error wins over activity' ERROR "$(status_state "$FIXTURES/status-error-active.txt")"
check_eq 'active evidence is proven' ACTIVE "$(status_evidence "$FIXTURES/status-active.txt")"
check_eq 'continuity error evidence is proven' CONTINUITY-ERROR "$(status_evidence "$FIXTURES/status-error-active.txt")"
check_eq 'unrecognized status is invalid' INVALID "$(status_evidence "$FIXTURES/status-invalid.txt")"
check_eq 'post-start activity is preserved' PRESERVE-CATCHING-UP "$(post_start_action ACTIVE)"
check_eq 'post-start continuity error is preserved' PRESERVE-ERROR "$(post_start_action CONTINUITY-ERROR)"
check_eq 'post-start invalid status rolls back' ROLLBACK "$(post_start_action INVALID)"
check_eq 'post-start inactive status rolls back' ROLLBACK "$(post_start_action IDLE)"
check_true 'non-writable parent mode accepted' directory_mode_safe 755
check_false 'group-writable parent mode rejected' directory_mode_safe 775
check_false 'world-writable parent mode rejected' directory_mode_safe 757
mode=600
directory_mode_safe 755
check_eq 'directory validation does not clobber caller mode' 600 "$mode"
check_eq 'running healthy classification' HEALTHY "$(classify_state 1 1 1 1 "$FIXTURES/status-healthy.txt")"
check_eq 'running mismatch fails closed' ERROR "$(classify_state 1 1 0 1 "$FIXTURES/status-healthy.txt")"
check_eq 'control without process fails closed' ERROR "$(classify_state 0 1 1 1 "$FIXTURES/status-healthy.txt")"
check_eq 'fully absent classification' ABSENT "$(classify_state 0 0 0 0 "$FIXTURES/status-healthy.txt")"
check_eq 'stopped installation is partial' PARTIAL "$(classify_state 0 0 1 1 "$FIXTURES/status-healthy.txt")"

check_true 'accepted pkg self-upgrade plan' validate_package_plan "$FIXTURES/pkg-plan-accepted.txt"
check_true 'newer zrepl package plan accepted' validate_package_plan "$FIXTURES/pkg-plan-newer.txt"
check_true 'zrepl upgrade package plan accepted' validate_package_plan "$FIXTURES/pkg-plan-upgrade.txt"
check_true 'pkg self-upgrade detected' package_plan_has_pkg_upgrade "$FIXTURES/pkg-plan-accepted.txt"
check_true 'isolated pkg self-upgrade accepted' validate_pkg_self_plan "$FIXTURES/pkg-plan-self-upgrade.txt"
check_false 'pkg self-upgrade dependency rejected' validate_pkg_self_plan "$FIXTURES/pkg-plan-self-upgrade-dependency.txt"
check_false 'dependency plan rejected' validate_package_plan "$FIXTURES/pkg-plan-dependency.txt"
check_false 'removal plan rejected' validate_package_plan "$FIXTURES/pkg-plan-removal.txt"
check_true 'script-free exact-origin package manifest accepted' validate_package_manifest "$FIXTURES/pkg-manifest-safe.ucl" filesystems/zrepl freebsd:14:x86:64 none
check_true 'newer script-free package manifest accepted' validate_package_manifest "$FIXTURES/pkg-manifest-newer.ucl" filesystems/zrepl freebsd:14:x86:64 none
check_eq 'audited Lua package scripts hashed' 6b7d6c91f960a4f9ed4d572f4a695900a116c802c8e14878ff40a1267255a494 "$(package_lua_scripts_hash "$FIXTURES/pkg-manifest-lua.ucl")"
check_true 'audited Lua package scripts accepted' validate_package_manifest "$FIXTURES/pkg-manifest-lua.ucl" filesystems/zrepl freebsd:14:x86:64 6b7d6c91f960a4f9ed4d572f4a695900a116c802c8e14878ff40a1267255a494
check_false 'changed Lua package scripts rejected' validate_package_manifest "$FIXTURES/pkg-manifest-lua.ucl" filesystems/zrepl freebsd:14:x86:64 0000000000000000000000000000000000000000000000000000000000000000
check_eq 'planned package version parsed' 0.7.0_7 "$(package_plan_zrepl_version "$FIXTURES/pkg-plan-newer.txt")"
check_eq 'upgrade target package version parsed' 0.7.0_7 "$(package_plan_zrepl_version "$FIXTURES/pkg-plan-upgrade.txt")"
check_eq 'candidate package version parsed' 0.7.0_7 "$(package_manifest_version "$FIXTURES/pkg-manifest-newer.ucl")"
check_true 'inspected candidate matches plan' package_candidate_matches_plan "$FIXTURES/pkg-plan-newer.txt" "$FIXTURES/pkg-manifest-newer.ucl"
check_false 'candidate version mismatch rejected' package_candidate_matches_plan "$FIXTURES/pkg-plan-accepted.txt" "$FIXTURES/pkg-manifest-newer.ucl"
check_true 'upgrade candidate matches target version' package_candidate_matches_plan "$FIXTURES/pkg-plan-upgrade.txt" "$FIXTURES/pkg-manifest-newer.ucl"
check_false 'upgrade candidate old version rejected' package_candidate_matches_plan "$FIXTURES/pkg-plan-upgrade.txt" "$FIXTURES/pkg-manifest-safe.ucl"
check_false 'package install script rejected' validate_package_manifest "$FIXTURES/pkg-manifest-scripted.ucl" filesystems/zrepl freebsd:14:x86:64 none
check_false 'wrong package origin rejected' validate_package_manifest "$FIXTURES/pkg-manifest-wrong-origin.ucl" filesystems/zrepl freebsd:14:x86:64 none
check_false 'wrong package architecture rejected' validate_package_manifest "$FIXTURES/pkg-manifest-wrong-arch.ucl" filesystems/zrepl freebsd:14:x86:64 none
check_eq 'package transaction size parsed' 44040192 "$(package_plan_required_bytes "$FIXTURES/pkg-plan-accepted.txt")"
check_false 'missing package transaction size rejected' package_plan_required_bytes "$FIXTURES/pkg-plan-dependency.txt"
check_true 'unchanged package payload accepted' validate_pkg_check_result /dev/null 0 0.7.0_5
check_true 'expected projected config mismatch accepted' validate_pkg_check_result "$FIXTURES/pkg-check-config-only.txt" 1 0.7.0_5
check_false 'tampered package binary rejected' validate_pkg_check_result "$FIXTURES/pkg-check-binary-tampered.txt" 1 0.7.0_5
check_eq 'network endpoint comes from reviewed YAML' '81.223.210.206 3479' "$(config_endpoint "$SOURCE_DIR/zrepl.yml")"
check_eq 'bounded snapshot marker' \
  'ZREPL_SNAPSHOT_V1 dataset=data1/replication/mali/personal snapshot=data1/replication/mali/personal@zrepl_20260816 guid=12345 creation=1786900000' \
  "$(snapshot_marker data1/replication/mali/personal data1/replication/mali/personal@zrepl_20260816 12345 1786900000)"
check_false 'unsafe snapshot marker rejected' snapshot_marker data1/replication/mali/personal 'data1/replication/mali/personal@bad name' 12345 1786900000
check_false 'cross-dataset snapshot marker rejected' snapshot_marker data1/replication/mali/personal data1/replication/mali/other@snap 12345 1786900000

result=$(emit_result ABSENT CATCHING-UP 1 v1-deadbeef-initial repaired pending)
check_eq 'bounded result record' \
  'ZREPL_BOOTSTRAP_V1 initial=ABSENT final=CATCHING-UP changed=1 bundle=v1-deadbeef-initial reason=repaired validation=pending' \
  "$result"
check_false 'free-text result reason rejected' emit_result ERROR ERROR 0 v1-deadbeef 'bad reason' fail

work=$(mktemp -d "${TMPDIR:-/tmp}/zrepl-bootstrap-tests.XXXXXX")
trap 'rm -rf "$work"' EXIT HUP INT TERM
mkdir -p "$work/package-fetch/All/Hashed"
printf 'candidate\n' >"$work/package-fetch/All/Hashed/zrepl.pkg"
check_eq 'hashed repository package layout accepted' "$work/package-fetch/All/Hashed/zrepl.pkg" "$(single_package_archive "$work/package-fetch")"
printf 'unexpected\n' >"$work/package-fetch/All/Hashed/other.pkg"
check_false 'multiple fetched package archives rejected' single_package_archive "$work/package-fetch"
rm -f "$work/package-fetch/All/Hashed/other.pkg"
mkdir "$work/root"
printf 'alpha\n' >"$work/root/a"
printf 'nested\n' >"$work/root/b"
printf 'a\nb\n' >"$work/paths"
write_manifest "$work/root" "$work/paths" "$work/manifest"
check_true 'generated manifest verifies' verify_manifest "$work/root" "$work/manifest" "$work/paths"
printf 'changed\n' >"$work/root/a"
check_false 'manifest catches changed payload' verify_manifest "$work/root" "$work/manifest" "$work/paths"
printf '%064d  ../escape\n' 0 >"$work/traversal"
check_false 'manifest rejects traversal' manifest_paths_safe "$work/traversal"

mkdir "$work/fakebin"
cat >"$work/fakebin/zfs" <<'EOF'
#!/bin/sh
for argument in "$@"; do dataset=$argument; done
case $dataset in *missing*) exit 1 ;; esac
case " $* " in
  *' -t snapshot '*) printf '%s@snap\t12345\t1786900000\n' "$dataset" ;;
  *) printf '%s\n' "$dataset" ;;
esac
EOF
chmod 0755 "$work/fakebin/zfs"
printf '%s\n' 'data1/replication/mali/present' >"$work/datasets-present"
printf '%s\n' 'data1/replication/mali/present' 'data1/replication/mali/missing' >"$work/datasets-missing"
printf '%s\n' 'data1/replication/mali/bad name' >"$work/datasets-malformed"
original_path=$PATH
PATH=$work/fakebin:$PATH
check_true 'declared datasets all present' validate_dataset_list "$work/datasets-present"
check_false 'missing declared dataset fails closed' validate_dataset_list "$work/datasets-missing"
check_false 'malformed declared dataset fails closed' validate_dataset_list "$work/datasets-malformed"
check_true 'bounded snapshot markers captured' capture_snapshot_markers "$work/datasets-present" "$work/markers"
check_eq 'captured marker count follows allow-list' 1 "$(wc -l <"$work/markers" | tr -d ' ')"
check_eq 'captured marker is bounded' \
  'ZREPL_SNAPSHOT_V1 dataset=data1/replication/mali/present snapshot=data1/replication/mali/present@snap guid=12345 creation=1786900000' \
  "$(cat "$work/markers")"
PATH=$original_path

property_block=$(sed -n '/^      properties:$/,/^      placeholder:$/p' "$SOURCE_DIR/zrepl.yml")
check_eq 'mixed filesystem and zvol receive properties are type-safe' \
  '      properties:
        inherit:
          - mountpoint
          - canmount
          - overlay
        override:
          readonly: on
      placeholder:' \
  "$property_block"
check_eq 'reviewed safe config checksum' \
  4c669c8bfc1be49abc535b8a74008dc9c9a049244bfff6f35b95d8c4c26db731 \
  "$(sha256_file "$SOURCE_DIR/zrepl.yml")"
check_eq 'sender and receiver are both keep-all' 2 \
  "$(grep -c 'regex: "\.\*"' "$SOURCE_DIR/zrepl.yml")"

check_false 'bootstrap has no continuity or ZFS-destructive mutation' grep -Eq \
  'zfs[[:space:]]+(destroy|rollback|receive)|zrepl[[:space:]]+signal|release-(all|stale)' \
  "$SOURCE_DIR/bootstrap.sh"
check_false 'bootstrap never prints TLS file content' grep -Eq \
  '(cat|sed|awk|head|tail)[[:space:]].*(rsyncnet\.key|mali\.crt|rsyncnet\.crt)' \
  "$SOURCE_DIR/bootstrap.sh"
check_true 'publisher selects only receiver-local private key copies' grep -Fq 'select_private_key_source' "$SOURCE_DIR/publish-release.sh"
check_false 'publisher accepts no key source argument' grep -Fq 'CERTIFICATE_SOURCE_DIR' "$SOURCE_DIR/publish-release.sh"
check_eq 'private manifest inventory is exact' 'tls/rsyncnet.key' "$(cat "$SOURCE_DIR/private-paths")"
check_false 'private key excluded from public and certificate inventories' grep -Fq 'tls/rsyncnet.key' \
  "$SOURCE_DIR/public-paths" "$SOURCE_DIR/certificate-paths"
check_eq 'single managed live-file inventory' 5 "$(wc -l <"$SOURCE_DIR/managed-files" | tr -d ' ')"
check_true 'bootstrap silently verifies receiver-local private manifest' grep -Fq 'manifest.private.sha256' "$SOURCE_DIR/bootstrap.sh"
# The searched text is intentionally literal shell source.
# shellcheck disable=SC2016
check_true 'backup pins rollback inventory authority' grep -Fq '"$BACKUP/managed-files"' "$SOURCE_DIR/bootstrap.sh"
# shellcheck disable=SC2016
check_true 'created parent directories have rollback records' grep -Fq '"$BACKUP/created-directories"' "$SOURCE_DIR/bootstrap.sh"
# shellcheck disable=SC2016
validation_line=$(grep -n 'validate_release "$release"' "$SOURCE_DIR/publish-release.sh" | tail -n 1 | cut -d: -f1)
# shellcheck disable=SC2016
repoint_line=$(grep -n 'mv -fh "$current_tmp"' "$SOURCE_DIR/publish-release.sh" | cut -d: -f1)
check_true 'existing release fully validates before current repoint' test "$validation_line" -lt "$repoint_line"
check_true 'human-only private-key recovery is documented' grep -Fq 'restore a lost key manually from 1Password' "$SOURCE_DIR/README.md"
check_false 'inert package policy removed' test -e "$SOURCE_DIR/package.policy"
check_false 'bundle metadata does not pin package version' grep -q '^package_version=' "$SOURCE_DIR/bundle.meta.in"
check_true 'archive architecture is pinned to package manifest format' grep -Fxq 'package_manifest_arch=freebsd:14:x86:64' "$SOURCE_DIR/bundle.meta.in"
check_true 'audited package Lua scripts are pinned' grep -Fxq 'package_lua_scripts_sha256=6b7d6c91f960a4f9ed4d572f4a695900a116c802c8e14878ff40a1267255a494' "$SOURCE_DIR/bundle.meta.in"
check_true 'unversioned package policy documented' grep -Fq 'does not pin a package version' "$SOURCE_DIR/README.md"
check_true 'post-start failures use rollback boundary' grep -Fq 'rollback_started_repair' "$SOURCE_DIR/bootstrap.sh"
check_true 'installed package origin and architecture are exact' grep -Fq "pkg query '%n|%v|%o|%q'" "$SOURCE_DIR/bootstrap.sh"
check_true 'required package paths have exact owner package' grep -Fq 'pkg which -q /usr/local/bin/zrepl' "$SOURCE_DIR/bootstrap.sh"
capacity_line=$(grep -n 'verify_repair_capacity' "$SOURCE_DIR/bootstrap.sh" | tail -n 1 | cut -d: -f1)
backup_line=$(grep -n 'backup_live_files ||' "$SOURCE_DIR/bootstrap.sh" | cut -d: -f1)
check_true 'transaction and backup capacity precede persistent backup' test "$capacity_line" -lt "$backup_line"
check_eq 'package dry-runs are noninteractive' 7 \
  "$(grep -Ec 'pkg install -n( -f)? -y (zrepl|pkg)' "$SOURCE_DIR/bootstrap.sh")"
forbidden_public_identifier=$(printf '\142\157\157\153\163\056\163\157\143\157\172\171\056\143\141\163\141')
check_false 'publication tree excludes forbidden private identifier' grep -rFq \
  "$forbidden_public_identifier" "$SOURCE_DIR"
printf 'ok - %s focused checks passed\n' "$passed"
