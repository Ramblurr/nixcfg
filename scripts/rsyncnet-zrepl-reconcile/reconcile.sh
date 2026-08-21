#!/usr/bin/env bash
set -euo pipefail
umask 077
export LC_ALL=C

expected_bundle=${EXPECTED_BUNDLE_ID:?}
expected_datasets=${EXPECTED_DATASETS_FILE:?}
receiver_alias=${RECEIVER_ALIAS:?}
receiver_host=${RECEIVER_HOST:?}
credential_directory=${CREDENTIALS_DIRECTORY:?}
identity_file=$credential_directory/identity
known_hosts_file=$credential_directory/known-hosts
state_directory=${STATE_DIRECTORY:?}
runtime_directory=${RUNTIME_DIRECTORY:?}
ssh_deadline_seconds=${SSH_DEADLINE_SECONDS:-900}
alias_valid=1
case $receiver_alias in
  '' | *[!A-Za-z0-9._-]*)
    alias_valid=0
    receiver_alias=invalid
    ;;
esac

now=$(date -u +%s)
timestamp=$(date -u +%Y%m%dT%H%M%SZ)

write_state() {
  local name=$1 value=$2 temporary
  temporary=$(mktemp "$state_directory/.${name}.XXXXXX")
  printf '%s\n' "$value" >"$temporary"
  chmod 0600 "$temporary"
  mv -f "$temporary" "$state_directory/$name"
}

state_file() {
  local path=$1
  test -f "$path" && test ! -L "$path" && test -r "$path" &&
    [[ $(stat -c %a "$path") == 600 ]]
}

secure_file() {
  local path=$1 owner mode
  test -f "$path" && test ! -L "$path" && test -r "$path" || return 1
  owner=$(stat -c %u "$path")
  mode=$(stat -c %a "$path")
  [[ $owner == 0 || $owner == "$(id -u)" ]] && [[ $mode == 400 || $mode == 600 ]]
}

read_number() {
  local path=$1 value
  state_file "$path" || return 1
  value=$(cat "$path")
  [[ $value =~ ^[0-9]{1,18}$ ]] || return 1
  printf '%s\n' "$value"
}

emit_local_result() {
  local outcome=$1 ssh_state=$2 state=$3 changed=$4 bundle=$5 reason=$6 validation=$7 advancement=$8 action=$9
  local failures=0 threshold=0 last_success line

  case $outcome in
    success)
      write_state failures 0
      write_state last-success "$now"
      ;;
    failure)
      failures=$(read_number "$state_directory/failures" 2>/dev/null || printf 0)
      failures=$((failures + 1))
      write_state failures "$failures"
      ;;
    retry)
      failures=$(read_number "$state_directory/failures" 2>/dev/null || printf 0)
      ;;
    *) exit 70 ;;
  esac

  if [[ $outcome != success ]]; then
    if ((failures >= 3)); then
      threshold=1
    elif last_success=$(read_number "$state_directory/last-success" 2>/dev/null) && ((now - last_success >= 2700)); then
      threshold=1
    fi
  fi

  line="ZREPL_RECONCILE_V1 time=$timestamp receiver=$receiver_alias ssh=$ssh_state state=$state changed=$changed bundle=$bundle reason=$reason validation=$validation advancement=$advancement failures=$failures threshold=$threshold action=$action"
  write_state last-result "$line"
  printf '%s\n' "$line"
}

fail_local() {
  emit_local_result failure "$1" "${2:-ERROR}" "${3:-0}" "${4:-unknown}" "$5" "${6:-fail}" "${7:-none}" "${8:-none}"
  exit 1
}

remote_output=$(mktemp "$runtime_directory/remote-output.XXXXXX")
remote_error=$(mktemp "$runtime_directory/remote-error.XXXXXX")
current_markers=$(mktemp "$runtime_directory/current-markers.XXXXXX")
seen=$(mktemp "$runtime_directory/seen-datasets.XXXXXX")
ssh_pid=
cleanup() {
  rm -f "$remote_output" "$remote_error" "$current_markers" "$seen"
}

terminate() {
  local status=$1
  trap - HUP INT TERM
  if [[ -n $ssh_pid ]]; then
    kill -TERM "$ssh_pid" 2>/dev/null || :
    wait "$ssh_pid" 2>/dev/null || :
    ssh_pid=
  fi
  emit_local_result failure terminated ERROR 0 unknown terminated fail none retry || :
  exit "$status"
}

trap cleanup EXIT
trap 'terminate 129' HUP
trap 'terminate 130' INT
trap 'terminate 143' TERM

host_re='^[A-Za-z0-9]([A-Za-z0-9-]*[A-Za-z0-9])?([.][A-Za-z0-9]([A-Za-z0-9-]*[A-Za-z0-9])?)*$'
if ((alias_valid == 0)) || [[ ! $receiver_host =~ $host_re ]]; then
  fail_local local ERROR 0 unknown config fail none none
fi
if [[ ! $ssh_deadline_seconds =~ ^[0-9]{1,4}$ ]] || ((ssh_deadline_seconds < 1 || ssh_deadline_seconds >= 1200)); then
  fail_local local ERROR 0 unknown config fail none none
fi
if ! secure_file "$identity_file" || ! test -r "$identity_file" ||
  ! secure_file "$known_hosts_file" || ! test -r "$known_hosts_file"; then
  fail_local credential ERROR 0 unknown credential fail none none
fi

expected_count=0
while IFS= read -r dataset; do
  [[ $dataset =~ ^data1/replication/mali/[A-Za-z0-9_./:%+-]+$ ]] || fail_local local ERROR 0 unknown config fail none none
  expected_count=$((expected_count + 1))
done <"$expected_datasets"
((expected_count > 0 && expected_count <= 8)) || fail_local local ERROR 0 unknown config fail none none
[[ $(sort -u "$expected_datasets" | wc -l) -eq $expected_count ]] || fail_local local ERROR 0 unknown config fail none none

ssh_status=0
timeout --signal=TERM --kill-after=5s "${ssh_deadline_seconds}s" ssh \
  -F none \
  -o AddKeysToAgent=no \
  -o BatchMode=yes \
  -o CanonicalizeHostname=no \
  -o ClearAllForwardings=yes \
  -o ConnectTimeout=10 \
  -o ConnectionAttempts=1 \
  -o ControlMaster=no \
  -o ControlPath=none \
  -o ControlPersist=no \
  -o GlobalKnownHostsFile=/dev/null \
  -o IdentitiesOnly=yes \
  -o "IdentityFile=$identity_file" \
  -o KbdInteractiveAuthentication=no \
  -o LogLevel=ERROR \
  -o NumberOfPasswordPrompts=0 \
  -o PasswordAuthentication=no \
  -o PermitLocalCommand=no \
  -o PreferredAuthentications=publickey \
  -o ProxyCommand=none \
  -o ProxyJump=none \
  -o RemoteCommand=none \
  -o RequestTTY=no \
  -o ServerAliveCountMax=40 \
  -o ServerAliveInterval=15 \
  -o StrictHostKeyChecking=yes \
  -o UpdateHostKeys=no \
  -o "UserKnownHostsFile=$known_hosts_file" \
  "root@$receiver_host" >"$remote_output" 2>"$remote_error" &
ssh_pid=$!
if wait "$ssh_pid"; then
  ssh_status=0
else
  ssh_status=$?
fi
ssh_pid=

if ((ssh_status == 124)); then
  fail_local timeout ERROR 0 unknown timeout fail none retry
fi
if ((ssh_status == 255)); then
  if grep -Eqi 'REMOTE HOST IDENTIFICATION HAS CHANGED|Host key verification failed' "$remote_error"; then
    fail_local host-key ERROR 0 unknown host-key fail none none
  fi
  fail_local transport ERROR 0 unknown transport fail none none
fi

if ((ssh_status == 126 || ssh_status == 127)) && ! test -s "$remote_output"; then
  fail_local ok ERROR 0 unknown bundle fail none human-restore
fi

if (($(wc -c <"$remote_output") > 16384)); then
  fail_local ok ERROR 0 unknown protocol fail none none
fi

marker_re='^ZREPL_SNAPSHOT_V1 dataset=([A-Za-z0-9_./:%+-]+) snapshot=([A-Za-z0-9_./:%+@-]+) guid=([0-9]+) creation=([0-9]+)$'
result_re='^ZREPL_BOOTSTRAP_V1 initial=(ABSENT|PARTIAL|HEALTHY|CATCHING-UP|ERROR) final=(ABSENT|PARTIAL|HEALTHY|CATCHING-UP|ERROR) changed=([01]) bundle=([A-Za-z0-9._-]+) reason=([A-Za-z0-9-]+) validation=(pass|pending|fail)$'
marker_count=0
result_count=0
: >"$current_markers"

while IFS= read -r line || [[ -n $line ]]; do
  if [[ $line =~ $marker_re ]] && ((result_count == 0)); then
    dataset=${BASH_REMATCH[1]}
    snapshot=${BASH_REMATCH[2]}
    [[ $snapshot == "$dataset@"* ]] || fail_local ok ERROR 0 unknown protocol fail none none
    grep -Fxq "$dataset" "$expected_datasets" || fail_local ok ERROR 0 unknown protocol fail none none
    ! grep -Fxq "$dataset" "$seen" || fail_local ok ERROR 0 unknown protocol fail none none
    printf '%s\n' "$dataset" >>"$seen"
    printf '%s\n' "$line" >>"$current_markers"
    marker_count=$((marker_count + 1))
  elif [[ $line =~ $result_re ]] && ((result_count == 0)); then
    initial=${BASH_REMATCH[1]}
    final=${BASH_REMATCH[2]}
    changed=${BASH_REMATCH[3]}
    bundle=${BASH_REMATCH[4]}
    reason=${BASH_REMATCH[5]}
    validation=${BASH_REMATCH[6]}
    result_count=1
  else
    fail_local ok ERROR 0 unknown protocol fail none none
  fi
done <"$remote_output"

((result_count == 1)) || fail_local ok ERROR 0 unknown protocol fail none none
case $reason in
  none | healthy | active | repaired | lock-busy | trust | mount | bundle | key | system | pool | capacity | network | package-plan | package-install | live-mismatch | config | service | control | job | snapshot | replication | rollback | internal) ;;
  *) fail_local ok ERROR 0 unknown protocol fail none none ;;
esac
bundle_ok=0
if [[ $bundle == "$expected_bundle" ]]; then
  bundle_ok=1
elif [[ $bundle == unknown && $initial == ERROR && $final == ERROR && $changed == 0 && $marker_count == 0 && $reason =~ ^(trust|mount|bundle)$ ]]; then
  bundle_ok=1
fi

protocol_ok=0
if ((bundle_ok == 1)); then
  case $ssh_status in
    0)
      case "$initial|$final|$changed|$reason|$validation|$marker_count" in
        "HEALTHY|HEALTHY|0|healthy|pass|$expected_count" | \
          "CATCHING-UP|CATCHING-UP|0|active|pending|$expected_count" | \
          "ABSENT|CATCHING-UP|1|repaired|pending|$expected_count" | \
          "PARTIAL|CATCHING-UP|1|repaired|pending|$expected_count") protocol_ok=1 ;;
      esac
      ;;
    75)
      if [[ $initial == ERROR && $final == ERROR && $changed == 0 && $bundle == "$expected_bundle" && $reason == lock-busy && $validation == pending && $marker_count == "$expected_count" ]]; then protocol_ok=1; fi
      ;;
    77)
      if [[ $initial == ERROR && $final == ERROR && $changed == 0 && $bundle == unknown && $reason == trust && $validation == fail && $marker_count == 0 ]]; then protocol_ok=1; fi
      ;;
    69)
      if [[ $validation == fail ]]; then
        if [[ $final == CATCHING-UP && $initial =~ ^(ABSENT|PARTIAL)$ && $changed == 1 && $bundle == "$expected_bundle" && $marker_count == "$expected_count" && $reason =~ ^(service|control|job|internal)$ ]]; then
          protocol_ok=1
        elif [[ $final == ERROR ]]; then
          case $initial in
            ERROR)
              if [[ $changed == 0 && $bundle == unknown && $marker_count == 0 && $reason =~ ^(mount|bundle)$ ]]; then
                protocol_ok=1
              elif [[ $changed == 0 && $bundle == "$expected_bundle" ]] &&
                { [[ $marker_count == 0 && $reason =~ ^(pool|snapshot)$ ]] ||
                  [[ $marker_count == "$expected_count" && $reason =~ ^(bundle|network|service|control|live-mismatch|replication)$ ]]; }; then
                protocol_ok=1
              fi
              ;;
            ABSENT | PARTIAL)
              if [[ $bundle == "$expected_bundle" ]] &&
                { [[ $marker_count == 0 && $reason == snapshot && $changed =~ ^[01]$ ]] ||
                  [[ $marker_count == "$expected_count" && $changed == 0 && $reason =~ ^(package-plan|capacity|config|live-mismatch|service|control)$ ]] ||
                  [[ $marker_count == "$expected_count" && $changed == 1 && $reason =~ ^(service|control|job|replication|internal)$ ]]; }; then
                protocol_ok=1
              fi
              ;;
            HEALTHY | CATCHING-UP)
              if [[ $bundle == "$expected_bundle" && $changed == 0 && $marker_count == 0 && $reason == snapshot ]]; then
                protocol_ok=1
              fi
              ;;
          esac
        fi
      fi
      ;;
    70)
      if [[ $validation == fail && $initial == ERROR && $final == ERROR && $changed == 0 && $bundle == unknown && $reason == bundle && $marker_count == 0 ]]; then
        protocol_ok=1
      elif [[ $validation == fail && $final == ERROR && $bundle == "$expected_bundle" && $marker_count == "$expected_count" ]]; then
        case $initial in
          ERROR) if [[ $changed == 0 && $reason == internal ]]; then protocol_ok=1; fi ;;
          ABSENT | PARTIAL) if [[ $reason == rollback || $changed == 1 && $reason == internal ]]; then protocol_ok=1; fi ;;
        esac
      fi
      ;;
  esac
fi
((protocol_ok == 1)) || fail_local ok ERROR 0 unknown protocol fail none none
cat "$remote_output"

if [[ $final == ERROR ]]; then
  if [[ $reason == lock-busy ]]; then
    emit_local_result retry ok "$final" 0 "$bundle" "$reason" pending none retry
    exit 75
  fi
  action=none
  if [[ $reason == bundle ]]; then action=human-restore; fi
  emit_local_result failure ok "$final" "$changed" "$bundle" "$reason" "$validation" none "$action"
  exit 1
fi

sort -o "$current_markers" "$current_markers"
baseline=$state_directory/advancement-baseline
deadline_file=$state_directory/advancement-deadline
advancement=none

if [[ $changed == 1 ]]; then
  temporary=$(mktemp "$state_directory/.advancement-baseline.XXXXXX")
  cat "$current_markers" >"$temporary"
  chmod 0600 "$temporary"
  mv -f "$temporary" "$baseline"
  write_state advancement-deadline "$((now + 2700))"
  advancement=baseline
elif state_file "$baseline"; then
  advanced=0
  while IFS= read -r dataset; do
    baseline_creation=$(awk -v target="dataset=$dataset" '$2 == target { sub(/^creation=/, "", $5); print $5 }' "$baseline")
    current_creation=$(awk -v target="dataset=$dataset" '$2 == target { sub(/^creation=/, "", $5); print $5 }' "$current_markers")
    [[ $baseline_creation =~ ^[0-9]+$ && $current_creation =~ ^[0-9]+$ ]] || fail_local ok ERROR 0 "$bundle" state fail fail none
    ((current_creation > baseline_creation)) && advanced=1
  done <"$expected_datasets"

  if ((advanced == 1)); then
    rm -f "$baseline" "$deadline_file"
    advancement=pass
  else
    deadline=$(read_number "$deadline_file" 2>/dev/null || printf 0)
    if ((deadline == 0 || now > deadline)); then
      emit_local_result failure ok ERROR 0 "$bundle" advancement fail fail none
      exit 1
    fi
    advancement=pending
  fi
elif test -e "$baseline" || test -L "$baseline"; then
  fail_local ok ERROR 0 "$bundle" state fail fail none
fi

if ((ssh_status == 0)); then
  emit_local_result success ok "$final" "$changed" "$bundle" "$reason" "$validation" "$advancement" none
else
  emit_local_result failure ok "$final" "$changed" "$bundle" "$reason" "$validation" "$advancement" none
  exit 1
fi
