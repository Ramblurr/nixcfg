#!/usr/bin/env bash
set -euo pipefail
umask 077

source_dir=$(CDPATH='' cd -- "$(dirname -- "$0")/.." && pwd)
repository_root=$(CDPATH='' cd -- "$source_dir/../.." && pwd)
work=$(mktemp -d "${TMPDIR:-/tmp}/zrepl-reconcile-test.XXXXXX")
trap 'rm -rf "$work"' EXIT HUP INT TERM
passed=0

check_true() {
  local name=$1
  shift
  if ! "$@"; then
    printf 'not ok - %s\n' "$name" >&2
    exit 1
  fi
  passed=$((passed + 1))
}

check_false() {
  local name=$1
  shift
  if "$@"; then
    printf 'not ok - %s\n' "$name" >&2
    exit 1
  fi
  passed=$((passed + 1))
}

check_eq() {
  local name=$1 expected=$2 actual=$3
  if [[ $actual != "$expected" ]]; then
    printf 'not ok - %s: expected <%s>, got <%s>\n' "$name" "$expected" "$actual" >&2
    exit 1
  fi
  passed=$((passed + 1))
}

mkdir "$work/bin" "$work/credentials" "$work/runtime" "$work/state"
printf 'not-a-real-key\n' >"$work/credentials/identity"
printf 'receiver.example.invalid ssh-ed25519 TEST\n' >"$work/credentials/known-hosts"
chmod 0600 "$work/credentials/identity" "$work/credentials/known-hosts"
cp "$repository_root/scripts/rsyncnet-zrepl-bootstrap/validation-datasets" "$work/datasets"

printf '#!%s\n' "$(command -v bash)" >"$work/bin/ssh"
cat >>"$work/bin/ssh" <<'EOF'
set -eu
printf '%s\n' "$@" >"$SSH_ARGS_LOG"
marker_lines() {
  offset=${1:-0}
  while IFS= read -r dataset; do
    printf 'ZREPL_SNAPSHOT_V1 dataset=%s snapshot=%s@zrepl_test_%s guid=%s creation=%s\n' \
      "$dataset" "$dataset" "$((100 + offset))" "$((1000 + offset))" "$((2000 + offset))"
  done <"$EXPECTED_DATASETS_FILE"
}
case $SCENARIO in
  healthy)
    marker_lines
    printf '%s\n' "ZREPL_BOOTSTRAP_V1 initial=HEALTHY final=HEALTHY changed=0 bundle=$EXPECTED_BUNDLE_ID reason=healthy validation=pass"
    ;;
  active)
    marker_lines
    printf '%s\n' "ZREPL_BOOTSTRAP_V1 initial=CATCHING-UP final=CATCHING-UP changed=0 bundle=$EXPECTED_BUNDLE_ID reason=active validation=pending"
    ;;
  advanced)
    marker_lines 1
    printf '%s\n' "ZREPL_BOOTSTRAP_V1 initial=CATCHING-UP final=CATCHING-UP changed=0 bundle=$EXPECTED_BUNDLE_ID reason=active validation=pending"
    ;;
  repair)
    marker_lines
    printf '%s\n' "ZREPL_BOOTSTRAP_V1 initial=ABSENT final=CATCHING-UP changed=1 bundle=$EXPECTED_BUNDLE_ID reason=repaired validation=pending"
    ;;
  preserved-active)
    marker_lines
    printf '%s\n' "ZREPL_BOOTSTRAP_V1 initial=PARTIAL final=CATCHING-UP changed=1 bundle=$EXPECTED_BUNDLE_ID reason=service validation=fail"
    exit 69
    ;;
  impossible-active)
    marker_lines
    printf '%s\n' "ZREPL_BOOTSTRAP_V1 initial=CATCHING-UP final=CATCHING-UP changed=0 bundle=$EXPECTED_BUNDLE_ID reason=active validation=pass"
    ;;
  impossible-change)
    marker_lines
    printf '%s\n' "ZREPL_BOOTSTRAP_V1 initial=HEALTHY final=HEALTHY changed=1 bundle=$EXPECTED_BUNDLE_ID reason=healthy validation=pass"
    ;;
  continuity-error)
    marker_lines
    printf '%s\n' "ZREPL_BOOTSTRAP_V1 initial=ERROR final=ERROR changed=0 bundle=$EXPECTED_BUNDLE_ID reason=replication validation=fail"
    exit 69
    ;;
  lock-busy)
    marker_lines
    printf '%s\n' "ZREPL_BOOTSTRAP_V1 initial=ERROR final=ERROR changed=0 bundle=$EXPECTED_BUNDLE_ID reason=lock-busy validation=pending"
    exit 75
    ;;
  bundle-error)
    printf '%s\n' 'ZREPL_BOOTSTRAP_V1 initial=ERROR final=ERROR changed=0 bundle=unknown reason=bundle validation=fail'
    exit 69
    ;;
  bundle-state-error)
    marker_lines
    printf '%s\n' "ZREPL_BOOTSTRAP_V1 initial=ERROR final=ERROR changed=0 bundle=$EXPECTED_BUNDLE_ID reason=bundle validation=fail"
    exit 69
    ;;
  bundle-lib-error)
    printf '%s\n' 'ZREPL_BOOTSTRAP_V1 initial=ERROR final=ERROR changed=0 bundle=unknown reason=bundle validation=fail'
    exit 70
    ;;
  missing-entrypoint)
    exit 127
    ;;
  timeout)
    exit 124
    ;;
  hung)
    : >"$HUNG_STARTED"
    printf '%s\n' "$$" >"$HUNG_PID"
    sleep 30
    ;;
  host-key)
    printf '%s\n' 'REMOTE HOST IDENTIFICATION HAS CHANGED leaked-detail' >&2
    exit 255
    ;;
  invalid)
    printf '%s\n' 'unbounded secret-like remote output'
    exit 70
    ;;
esac
EOF
chmod +x "$work/bin/ssh"

export EXPECTED_BUNDLE_ID=v1-c337a0f46626b904-initial
export EXPECTED_DATASETS_FILE=$work/datasets
export RECEIVER_ALIAS=rsyncnet
export RECEIVER_HOST=receiver.example.invalid
export CREDENTIALS_DIRECTORY=$work/credentials
export STATE_DIRECTORY=$work/state
export RUNTIME_DIRECTORY=$work/runtime
export SSH_ARGS_LOG=$work/ssh-args
export HUNG_STARTED=$work/hung-started
export HUNG_PID=$work/hung-pid
export SSH_DEADLINE_SECONDS=1
export PATH=$work/bin:$PATH

run_success() {
  export SCENARIO=$1
  if ! bash "$source_dir/reconcile.sh" >"$work/output" 2>&1; then
    cat "$work/output" >&2
    return 1
  fi
}

run_failure() {
  export SCENARIO=$1
  if bash "$source_dir/reconcile.sh" >"$work/output" 2>&1; then
    return 1
  fi
}

run_retry() {
  local status=0
  export SCENARIO=$1
  bash "$source_dir/reconcile.sh" >"$work/output" 2>&1 || status=$?
  [[ $status == 75 ]]
}
run_success healthy
check_true 'healthy receiver is a successful no-op' grep -Eq 'state=HEALTHY changed=0 .* advancement=none' "$work/output"
check_eq 'SSH requests no remote command' "root@$RECEIVER_HOST" "$(tail -n 1 "$SSH_ARGS_LOG")"
check_true 'SSH pins host identity' grep -Fxq "UserKnownHostsFile=$CREDENTIALS_DIRECTORY/known-hosts" "$SSH_ARGS_LOG"
check_true 'SSH disables arbitrary forwarding' grep -Fxq ClearAllForwardings=yes "$SSH_ARGS_LOG"
check_false 'healthy run creates no advancement baseline' test -e "$STATE_DIRECTORY/advancement-baseline"

RECEIVER_HOST='bad host'
export RECEIVER_HOST
run_failure healthy
check_true 'malformed receiver host emits bounded configuration failure' grep -Eq 'receiver=rsyncnet ssh=local state=ERROR changed=0 bundle=unknown reason=config' "$work/output"
check_eq 'malformed host failure is persisted' "$(tail -n 1 "$work/output")" "$(cat "$STATE_DIRECTORY/last-result")"
RECEIVER_HOST=receiver.example.invalid
export RECEIVER_HOST
run_success active
check_true 'active catch-up is a successful no-op' grep -Eq 'state=CATCHING-UP changed=0 .* advancement=none' "$work/output"

run_failure continuity-error
check_true 'continuity error is report-only' grep -Eq 'state=ERROR changed=0 .* reason=replication' "$work/output"
check_false 'continuity error creates no baseline' test -e "$STATE_DIRECTORY/advancement-baseline"

run_failure preserved-active
check_true 'nonzero fail-safe activity preserves state and mutation evidence' grep -Eq 'state=CATCHING-UP changed=1 .* reason=service validation=fail advancement=baseline' "$work/output"
run_failure impossible-active
check_true 'impossible active validation combination is rejected' grep -Fq 'reason=protocol' "$work/output"
run_failure impossible-change
check_true 'impossible healthy mutation combination is rejected' grep -Fq 'reason=protocol' "$work/output"
run_failure bundle-state-error
check_true 'persistent bootstrap state-directory failure remains bounded' grep -Eq 'bundle=v1-.* reason=bundle .* action=human-restore' "$work/output"
run_failure bundle-lib-error
check_true 'missing committed bundle library remains human-restore failure' grep -Eq 'reason=bundle .* action=human-restore' "$work/output"
run_success repair
check_true 'successful repair records bounded baseline' grep -Eq 'state=CATCHING-UP changed=1 .* advancement=baseline' "$work/output"
check_true 'repair baseline is private state' test "$(stat -c %a "$STATE_DIRECTORY/advancement-baseline")" = 600

rm -rf "$RUNTIME_DIRECTORY"
mkdir "$RUNTIME_DIRECTORY"
run_success active
check_true 'persisted baseline survives runtime-directory recreation' grep -Fq 'advancement=pending' "$work/output"
run_success advanced
check_true 'newer representative snapshot proves advancement' grep -Fq 'advancement=pass' "$work/output"
check_false 'passed advancement clears baseline' test -e "$STATE_DIRECTORY/advancement-baseline"

run_success repair
printf '0\n' >"$STATE_DIRECTORY/advancement-deadline"
run_failure active
check_true 'expired advancement window reports without remote mutation' grep -Eq 'state=ERROR changed=0 .* reason=advancement validation=fail advancement=fail' "$work/output"

run_failure bundle-error
check_true 'missing receiver-local bundle or key requests human recovery' grep -Fq 'reason=bundle validation=fail advancement=none failures=' "$work/output"
check_true 'bundle failure action is human restoration' grep -Fq 'action=human-restore' "$work/output"
run_retry lock-busy
check_true 'lock contention preserves exact retry evidence' grep -Eq 'state=ERROR changed=0 .* reason=lock-busy validation=pending advancement=none failures=2 threshold=0 action=retry' "$work/output"
check_eq 'lock contention does not increment failure counter' 2 "$(cat "$STATE_DIRECTORY/failures")"

run_failure missing-entrypoint
check_true 'absent forced entrypoint fails closed' grep -Eq 'bundle=unknown reason=bundle .* action=human-restore' "$work/output"
check_true 'three consecutive failures raise bounded threshold' grep -Fq 'threshold=1' "$work/output"

run_failure timeout
check_true 'SSH deadline is classified as bounded retryable failure' grep -Eq 'ssh=timeout state=ERROR .* reason=timeout .* action=retry' "$work/output"
check_eq 'SSH timeout failure is persisted' "$(tail -n 1 "$work/output")" "$(cat "$STATE_DIRECTORY/last-result")"
rm -f "$HUNG_STARTED" "$HUNG_PID"
run_failure hung
check_true 'hung SSH is ended by the real command deadline' grep -Eq 'ssh=timeout state=ERROR .* reason=timeout .* action=retry' "$work/output"
check_eq 'hung SSH deadline result is persisted' "$(tail -n 1 "$work/output")" "$(cat "$STATE_DIRECTORY/last-result")"

failures_before_termination=$(cat "$STATE_DIRECTORY/failures")
rm -f "$HUNG_STARTED" "$HUNG_PID"
export SCENARIO=hung SSH_DEADLINE_SECONDS=10
bash "$source_dir/reconcile.sh" >"$work/output" 2>&1 &
reconcile_pid=$!
for _ in {1..100}; do
  [[ -e $HUNG_STARTED ]] && break
  sleep 0.01
done
check_true 'termination test reached the SSH session' test -e "$HUNG_STARTED"
kill -TERM "$reconcile_pid"
termination_status=0
wait "$reconcile_pid" || termination_status=$?
export SSH_DEADLINE_SECONDS=1
check_eq 'termination preserves the conventional exit status' 143 "$termination_status"
check_false 'termination reaps the active SSH process' test -e "/proc/$(cat "$HUNG_PID")/status"
expected_termination_failures=$((failures_before_termination + 1))
check_eq 'termination increments the persisted failure counter once' "$expected_termination_failures" "$(cat "$STATE_DIRECTORY/failures")"
check_true 'termination emits exact bounded retry and threshold evidence' grep -Eq "ssh=terminated state=ERROR changed=0 bundle=unknown reason=terminated validation=fail advancement=none failures=$expected_termination_failures threshold=1 action=retry" "$work/output"
check_eq 'termination result is persisted' "$(tail -n 1 "$work/output")" "$(cat "$STATE_DIRECTORY/last-result")"
run_failure host-key
check_true 'host-key mismatch has bounded classification' grep -Fq 'ssh=host-key' "$work/output"
check_false 'raw SSH error is never logged' grep -Fq leaked-detail "$work/output"

run_failure invalid
check_true 'invalid remote output is rejected' grep -Fq 'reason=protocol' "$work/output"
check_false 'invalid remote output is not replayed' grep -Fq secret-like "$work/output"

check_true 'module remains disabled without private prerequisites' grep -Fq 'default = false;' "$repository_root/hosts/mali/zrepl-receiver-reconcile.nix"
check_false 'Mali reconciler never names receiver TLS key material' grep -Fq 'rsyncnet.key' \
  "$source_dir/reconcile.sh" "$repository_root/hosts/mali/zrepl-receiver-reconcile.nix"
check_false 'reconciler contains no destructive or wakeup command' grep -Eq \
  'zfs[[:space:]]+(destroy|rollback|receive)|zrepl[[:space:]]+signal|service[[:space:]]+zrepl|pkg[[:space:]]+(install|upgrade)' \
  "$source_dir/reconcile.sh"
# Assert the literal production variable reference.
# shellcheck disable=SC2016
check_true 'SSH command has deadline below systemd timeout' grep -Fq \
  'timeout --signal=TERM --kill-after=5s "${ssh_deadline_seconds}s" ssh' "$source_dir/reconcile.sh"

printf 'ok - %s focused checks passed\n' "$passed"
