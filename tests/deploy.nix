{
  deploy,
  pkgs,
}:
pkgs.runCommand "deploy-preserves-activation-failures-and-addams-transport" { } ''
  fakeBin="$TMPDIR/fake-bin"
  export remoteBin="$TMPDIR/remote-bin"
  export unitDir="$TMPDIR/units"
  export dropFile="$TMPDIR/drops"
  export hangFile="$TMPDIR/hangs"
  export partialFile="$TMPDIR/partials"
  export showFailFile="$TMPDIR/show-failures"
  export activatingFile="$TMPDIR/activating-polls"
  export earlyStopFile="$TMPDIR/stopped-while-activating"
  export controlLog="$TMPDIR/control.log"
  export activationLog="$TMPDIR/activation.log"
  export workRoot="$TMPDIR/work"
  export fakeSystem="$TMPDIR/fake-system"
  export sshLog="$TMPDIR/ssh.log"
  export nixLog="$TMPDIR/nix.log"
  export nvdLog="$TMPDIR/nvd.log"
  export sudoLog="$TMPDIR/sudo.log"
  export localHost="$(${pkgs.coreutils}/bin/uname -n)"
  mkdir -p "$fakeBin" "$remoteBin" "$unitDir" "$workRoot" "$fakeSystem/bin"
  touch "$workRoot/flake.nix" "$sshLog" "$nixLog" "$nvdLog" "$sudoLog" "$activationLog" "$controlLog"

  cat > "$fakeBin/git" <<'EOF'
  #!/bin/sh
  if [ "$1" = "rev-parse" ]; then
    printf '%s\n' "$workRoot"
    exit 0
  fi
  exit 64
  EOF

  cat > "$fakeBin/nom" <<'EOF'
  #!/bin/sh
  printf '%s\n' "$fakeSystem"
  EOF

  cat > "$fakeBin/nix" <<'EOF'
  #!/bin/sh
  printf '%s\n' "$*" >> "$nixLog"
  if [ "$1" = "eval" ]; then
    printf '%s\n' '{
      "debord":{"guest":false,"buildAttribute":"nixosConfigurations.debord.config.system.build.toplevel"},
      "quine":{"guest":false,"buildAttribute":"nixosConfigurations.quine.config.system.build.toplevel"},
      "addams":{"guest":false,"buildAttribute":"nixosConfigurations.addams.config.system.build.toplevel"},
      "thinkpad1":{"guest":false,"buildAttribute":"nixosConfigurations.thinkpad1.config.system.build.toplevel"}
    }' | ${pkgs.jq}/bin/jq --arg host "$localHost" \
      '. + {($host): {guest: false, buildAttribute: ("nixosConfigurations." + $host + ".config.system.build.toplevel")}}'
    exit 0
  fi
  if [ "$1" = "copy" ]; then
    printf 'NIX_SSHOPTS=%s\n' "$NIX_SSHOPTS" >> "$nixLog"
    target="''${4#ssh://}"
    [ "$(tail -n 1 "$sshLog")" = "$target|true" ] || exit 65
    exit 0
  fi
  exit 64
  EOF

  cat > "$fakeBin/ssh" <<'EOF'
  #!/bin/sh
  controlPath=none
  while [ "$1" = "-o" ]; do
    case "$2" in
      ControlPath=*) controlPath="''${2#ControlPath=}" ;;
    esac
    shift 2
  done
  if [ "$1" = "-O" ]; then
    printf '%s|-O %s %s\n' "$3" "$2" "$controlPath" >> "$sshLog"
    exit 0
  fi
  printf '%s\n' "$controlPath" >> "$controlLog"
  host="$1"
  shift
  if [ "$1" = "--" ]; then
    shift
  fi
  printf '%s|%s\n' "$host" "$*" >> "$sshLog"

  case "$1" in
    true)
      exit "''${authStatus:-0}"
      ;;
    /run/current-system/sw/bin/nix-env)
      exit 0
      ;;
  esac
  take() {
    count=$(cat "$1" 2>/dev/null || echo 0)
    [ "$count" -gt 0 ] && echo $((count - 1)) > "$1"
  }
  case "$*" in
    "systemctl show "*)
      ! take "$dropFile" || exit 255
      ! take "$hangFile" || exec sleep 200
      if take "$partialFile"; then
        printf 'LoadState=loa'
        exit 255
      fi
      ;;
  esac
  TARGET_HOST="''${host#root@}" PATH="$remoteBin:$PATH" exec sh -c "$*"
  EOF

  cat > "$remoteBin/systemd-run" <<'EOF'
  #!/bin/sh
  for arg; do
    shift
    case "$arg" in
      --unit=*) unit="''${arg#--unit=}" ;;
      --) break ;;
    esac
  done
  "$@" > "$unitDir/$unit.log" 2>&1
  echo $? > "$unitDir/$unit.status"
  echo "Running as unit: $unit"
  EOF

  cat > "$remoteBin/systemctl" <<'EOF'
  #!/bin/sh
  take() {
    count=$(cat "$1" 2>/dev/null || echo 0)
    [ "$count" -gt 0 ] && echo $((count - 1)) > "$1"
  }
  case "$1" in
    show)
      ! take "$showFailFile" || exit 1
      if take "$activatingFile"; then
        printf 'LoadState=loaded\nActiveState=activating\nResult=success\nExecMainStatus=0\nInactiveExitTimestampMonotonic=1\n'
      elif ! status=$(cat "$unitDir/$3.status" 2>/dev/null); then
        printf 'LoadState=not-found\nActiveState=inactive\nSubState=dead\nResult=success\nExecMainStatus=0\nInactiveExitTimestampMonotonic=0\n'
      elif [ "$status" = 0 ]; then
        printf 'LoadState=loaded\nActiveState=active\nSubState=exited\nResult=success\nExecMainStatus=0\nInactiveExitTimestampMonotonic=1\n'
      else
        printf 'LoadState=loaded\nActiveState=failed\nSubState=failed\nResult=exit-code\nExecMainStatus=%s\nInactiveExitTimestampMonotonic=1\n' "$status"
      fi
      ;;
    stop)
      [ "$(cat "$activatingFile" 2>/dev/null || echo 0)" -eq 0 ] || touch "$earlyStopFile"
      if [ -n "''${cleanupBusFail-}" ]; then
        echo 1 > "$showFailFile"
        exit 1
      fi
      [ -z "''${cleanupFail-}" ] || exit 1
      [ "$(cat "$unitDir/$2.status" 2>/dev/null)" != 0 ] || rm -f "$unitDir/$2".*
      ;;
    reset-failed)
      [ -z "''${cleanupFail-}''${cleanupBusFail-}" ] || exit 1
      rm -f "$unitDir/$2".*
      ;;
    *)
      exit 64
      ;;
  esac
  EOF

  cat > "$remoteBin/journalctl" <<'EOF'
  #!/bin/sh
  [ "$1" != "--sync" ] || exit 0
  [ -z "''${journalStatus-}" ] || exit "$journalStatus"
  after=0
  for arg; do
    case "$arg" in
      --unit=*) unit="''${arg#--unit=}" ;;
      --after-cursor=*) after="''${arg#*;i=}" ;;
    esac
  done
  total=$(wc -l < "$unitDir/$unit.log")
  tail -n +"$((after + 1))" "$unitDir/$unit.log"
  [ "$total" -le "$after" ] || echo "-- cursor: s=fake;i=$total"
  EOF

  cat > "$fakeSystem/bin/switch-to-configuration" <<'EOF'
  #!/bin/sh
  printf '%s %s\n' "$TARGET_HOST" "$1" >> "$activationLog"
  if [ "$TARGET_HOST" = debord ]; then
    echo "activation error on debord" >&2
    exit 42
  fi
  echo "activated $TARGET_HOST"
  EOF

  cat > "$fakeBin/readlink" <<'EOF'
  #!/bin/sh
  printf '%s\n' /nix/store/previous-system
  EOF

  cat > "$fakeBin/sudo" <<'EOF'
  #!/bin/sh
  printf '%s\n' "$*" >> "$sudoLog"
  case "$1" in
    /run/current-system/sw/bin/nix-env)
      exit 0
      ;;
    "$fakeSystem/bin/switch-to-configuration")
      exit 42
      ;;
  esac
  exit 64
  EOF

  cat > "$fakeBin/nvd" <<'EOF'
  #!/bin/sh
  printf '%s\n' "$*" >> "$nvdLog"
  EOF


  chmod +x "$fakeBin"/* "$remoteBin"/* "$fakeSystem/bin/switch-to-configuration"

  assert_no_units() {
    [ -z "$(ls -A "$unitDir")" ] || {
      ls "$unitDir" >&2
      echo "deploy left transient units behind" >&2
      exit 1
    }
  }

  run_expected_failure() {
    host="$1"
    expected="$2"
    set +e
    PATH="$fakeBin:$PATH" ${deploy}/bin/deploy switch "$host" > "$TMPDIR/$host.stdout" 2> "$TMPDIR/$host.stderr"
    status=$?
    set -e

    if [ "$status" -eq 0 ]; then
      cat "$TMPDIR/$host.stderr" >&2
      echo "deploy returned success for expected failure on $host" >&2
      exit 1
    fi

    grep -F "$expected" "$TMPDIR/$host.stderr" || {
      cat "$TMPDIR/$host.stderr" >&2
      exit 1
    }
  }

  # The command comes first and is required; without hosts, help lists them.
  PATH="$fakeBin:$PATH" ${deploy}/bin/deploy > "$TMPDIR/bare.stdout"
  grep -Fx "  debord" "$TMPDIR/bare.stdout"
  if PATH="$fakeBin:$PATH" ${deploy}/bin/deploy switch > "$TMPDIR/nohosts.stdout"; then
    echo "deploy without hosts returned success" >&2
    exit 1
  fi
  grep -Fx "  debord" "$TMPDIR/nohosts.stdout"
  if PATH="$fakeBin:$PATH" ${deploy}/bin/deploy debord switch 2> "$TMPDIR/oldorder.stderr"; then
    echo "deploy accepted the host before the command" >&2
    exit 1
  fi
  grep -F "error: Unknown command: debord" "$TMPDIR/oldorder.stderr"
  [ ! -s "$sshLog" ] || { echo "rejected command reached SSH" >&2; exit 1; }

  # diff compares each host's running system and activates nothing.
  PATH="$fakeBin:$PATH" ${deploy}/bin/deploy diff debord "$localHost" addams thinkpad1 > "$TMPDIR/diff.stdout" 2> "$TMPDIR/diff.stderr"
  grep -Fx "root@debord|nvd --color always diff /run/current-system $fakeSystem" "$sshLog"
  grep -Fx "root@addams-lan|nvd --color always diff /run/current-system $fakeSystem" "$sshLog"
  grep -Fx "root@thinkpad1|nvd --color always diff /run/current-system $fakeSystem" "$sshLog"
  grep -Fx -- "--color always diff /run/current-system $fakeSystem" "$nvdLog"
  [ ! -s "$sudoLog" ] || { echo "diff ran sudo locally" >&2; exit 1; }
  if grep -q 'nix-env\|switch-to-configuration\|tpm-deploy\|readlink' "$sshLog"; then
    echo "diff activated or inspected a profile" >&2
    exit 1
  fi
  : > "$sshLog"

  run_expected_failure debord "error: Failed to activate debord"
  grep -Fx "activation error on debord" "$TMPDIR/debord.stdout"
  grep -F "ExecMainStatus=42" "$TMPDIR/debord.stderr"
  assert_no_units
  run_expected_failure "$localHost" "error: Failed to activate $localHost"
  if grep -q "^root@$localHost|" "$sshLog"; then
    echo "local deployment unexpectedly used SSH" >&2
    exit 1
  fi

  PATH="$fakeBin:$PATH" ${deploy}/bin/deploy switch addams > "$TMPDIR/addams.stdout" 2> "$TMPDIR/addams.stderr"
  grep -F "ssh://root@addams-lan" "$nixLog"
  grep -Fx "root@addams-lan|readlink -e /nix/var/nix/profiles/system" "$sshLog"
  grep -Fx "addams-lan switch" "$activationLog"
  grep -Fx "activated addams-lan" "$TMPDIR/addams.stdout"
  grep -Fx -- "--color always diff /nix/store/previous-system $fakeSystem" "$nvdLog"
  assert_no_units
  if grep -q '^addams|' "$sshLog" || grep -q '^root@addams|' "$sshLog"; then
    echo "addams deployment used its Tailscale SSH target" >&2
    exit 1
  fi

  : > "$sshLog"
  : > "$nixLog"
  : > "$controlLog"
  : > "$activationLog"
  echo 2 > "$dropFile"
  PATH="$fakeBin:$PATH" ${deploy}/bin/deploy switch addams > "$TMPDIR/drop.stdout" 2> "$TMPDIR/drop.stderr"
  [ "$(cat "$dropFile")" = 0 ] || { echo "polls were not retried" >&2; exit 1; }
  grep -Fx "addams-lan switch" "$activationLog"
  grep -Fx "activated addams-lan" "$TMPDIR/drop.stdout"
  assert_no_units

  controlPath=$(sort -u "$controlLog")
  case "$controlPath" in
    /dev/shm/nixcfg-deploy.*/cm-%C|/tmp/nixcfg-deploy.*/cm-%C) ;;
    *) echo "SSH calls did not share one whitespace-free deploy-owned ControlPath: $controlPath" >&2; exit 1 ;;
  esac
  grep -Fx "root@addams-lan|-O exit $controlPath" "$sshLog"
  if grep -F "|-O " "$sshLog" | grep -Fvx "root@addams-lan|-O exit $controlPath"; then
    echo "ssh -O reached a master deploy does not own" >&2
    exit 1
  fi
  grep -Fx "NIX_SSHOPTS=-o ConnectTimeout=5 -o ServerAliveInterval=5 -o ServerAliveCountMax=2 -o ControlMaster=auto -o ControlPath=$controlPath -o ControlPersist=300" "$nixLog"
  [ ! -e "$(dirname "$controlPath")" ] || { echo "deploy left its control directory behind" >&2; exit 1; }

  spacedTmp="$TMPDIR/dir with spaces"
  mkdir -p "$spacedTmp"
  : > "$controlLog"
  TMPDIR="$spacedTmp" PATH="$fakeBin:$PATH" ${deploy}/bin/deploy switch addams > "$TMPDIR/spaced.stdout" 2> "$TMPDIR/spaced.stderr"
  spacedControlPath=$(sort -u "$controlLog")
  case "$spacedControlPath" in
    *[[:space:]]*) echo "ControlPath contained whitespace under spaced TMPDIR: $spacedControlPath" >&2; exit 1 ;;
  esac
  grep -Fx "activated addams-lan" "$TMPDIR/spaced.stdout"
  assert_no_units

  : > "$activationLog"
  echo 1 > "$hangFile"
  hangStart=$SECONDS
  PATH="$fakeBin:$PATH" ${deploy}/bin/deploy switch addams > "$TMPDIR/hang.stdout" 2> "$TMPDIR/hang.stderr"
  [ $((SECONDS - hangStart)) -lt 60 ] || { echo "hung poll was not cut short by timeout" >&2; exit 1; }
  [ "$(cat "$hangFile")" = 0 ] || { echo "hung poll was not retried" >&2; exit 1; }
  grep -Fx "activated addams-lan" "$TMPDIR/hang.stdout"
  assert_no_units

  echo 3 > "$showFailFile"
  PATH="$fakeBin:$PATH" ${deploy}/bin/deploy switch addams > "$TMPDIR/showfail.stdout" 2> "$TMPDIR/showfail.stderr"
  [ "$(cat "$showFailFile")" = 0 ] || { echo "failed probes were not retried" >&2; exit 1; }
  grep -Fx "activated addams-lan" "$TMPDIR/showfail.stdout"
  assert_no_units

  echo 1 > "$partialFile"
  echo 2 > "$activatingFile"
  PATH="$fakeBin:$PATH" ${deploy}/bin/deploy switch addams > "$TMPDIR/partial.stdout" 2> "$TMPDIR/partial.stderr"
  [ "$(cat "$partialFile")" = 0 ] || { echo "partial probe was not retried" >&2; exit 1; }
  [ ! -e "$earlyStopFile" ] || { echo "deploy stopped a running activation" >&2; exit 1; }
  grep -Fx "activated addams-lan" "$TMPDIR/partial.stdout"
  assert_no_units

  journalStatus=1 PATH="$fakeBin:$PATH" ${deploy}/bin/deploy switch addams > "$TMPDIR/nojournal.stdout" 2> "$TMPDIR/nojournal.stderr"
  grep -E "^warning: failed to fetch final journal output for nixcfg-deploy-addams-.*\.service on addams \(exit 1\)$" "$TMPDIR/nojournal.stderr"
  assert_no_units

  cleanupFail=1 PATH="$fakeBin:$PATH" ${deploy}/bin/deploy switch addams > "$TMPDIR/cleanupfail.stdout" 2> "$TMPDIR/cleanupfail.stderr"
  grep -E "^warning: failed to clean up nixcfg-deploy-addams-.*\.service on addams$" "$TMPDIR/cleanupfail.stderr"
  grep -Fx "activated addams-lan" "$TMPDIR/cleanupfail.stdout"
  rm -f "$unitDir"/*

  cleanupBusFail=1 PATH="$fakeBin:$PATH" ${deploy}/bin/deploy switch addams > "$TMPDIR/cleanupbus.stdout" 2> "$TMPDIR/cleanupbus.stderr"
  grep -E "^warning: failed to clean up nixcfg-deploy-addams-.*\.service on addams$" "$TMPDIR/cleanupbus.stderr"
  grep -Fx "activated addams-lan" "$TMPDIR/cleanupbus.stdout"
  rm -f "$unitDir"/*

  : > "$sshLog"
  PATH="$fakeBin:$PATH" ${deploy}/bin/deploy dry-activate addams > "$TMPDIR/preview.stdout" 2> "$TMPDIR/preview.stderr"
  if grep -q 'nix-env' "$sshLog"; then
    echo "dry-activate changed the system profile" >&2
    exit 1
  fi

  # Test the CLI's failure propagation, not TPM behavior (covered by the VM).
  : > "$sshLog"
  run_expected_failure thinkpad1 "error: Missing TPM deployment helper for thinkpad1"
  if grep -q 'nix-env\|switch-to-configuration' "$sshLog"; then
    echo "missing TPM helper fell through to ordinary activation" >&2
    exit 1
  fi

  printf '#!/bin/sh\necho "tpm refused $1"\nexit 42\n' > "$fakeSystem/thinkpad1-tpm-deploy"
  chmod +x "$fakeSystem/thinkpad1-tpm-deploy"
  : > "$sshLog"
  run_expected_failure thinkpad1 "error: Failed TPM-aware activation of thinkpad1"
  grep -Fx "tpm refused switch" "$TMPDIR/thinkpad1.stdout"
  assert_no_units
  if grep -q 'nix-env\|switch-to-configuration' "$sshLog"; then
    echo "TPM-aware deployment fell through to ordinary activation" >&2
    exit 1
  fi

  : > "$sshLog"
  : > "$nixLog"
  export authStatus=42
  run_expected_failure addams "error: Failed to authenticate to addams"
  if grep -q '^copy ' "$nixLog" || grep -q 'nix-env\|switch-to-configuration' "$sshLog"; then
    echo "authentication failure did not stop deployment before copying/activation" >&2
    exit 1
  fi

  touch "$out"
''
