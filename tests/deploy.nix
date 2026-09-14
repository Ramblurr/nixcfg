{
  deploy,
  pkgs,
}:
pkgs.runCommand "deploy-preserves-activation-failures-and-addams-transport" { } ''
  fakeBin="$TMPDIR/fake-bin"
  export workRoot="$TMPDIR/work"
  export fakeSystem="$TMPDIR/fake-system"
  export sshLog="$TMPDIR/ssh.log"
  export nixLog="$TMPDIR/nix.log"
  export localHost="$(${pkgs.coreutils}/bin/uname -n)"
  mkdir -p "$fakeBin" "$workRoot" "$fakeSystem/bin"
  touch "$workRoot/flake.nix" "$sshLog" "$nixLog"

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
    target="''${4#ssh://}"
    [ "$(tail -n 1 "$sshLog")" = "$target|true" ] || exit 65
    exit 0
  fi
  exit 64
  EOF

  cat > "$fakeBin/ssh" <<'EOF'
  #!/bin/sh
  host="$1"
  shift
  if [ "$1" = "--" ]; then
    shift
  fi
  printf '%s|%s\n' "$host" "$*" >> "$sshLog"

  case "$host:$1" in
    root@debord:readlink|root@addams-lan:readlink|root@thinkpad1:readlink)
      printf '%s\n' /nix/store/previous-system
      exit 0
      ;;
  esac

  case "$host:$1" in
    root@*:true)
      exit "''${authStatus:-0}"
      ;;
    root@thinkpad1:"$fakeSystem/thinkpad1-tpm-deploy")
      exit 42
      ;;
    root@debord:/run/current-system/sw/bin/nix-env|root@addams-lan:/run/current-system/sw/bin/nix-env)
      exit 0
      ;;
    root@debord:"$fakeSystem/bin/switch-to-configuration")
      exit 42
      ;;
    root@addams-lan:"$fakeSystem/bin/switch-to-configuration"|root@addams-lan:nvd|root@debord:nvd)
      exit 0
      ;;
  esac

  exit 64
  EOF

  cat > "$fakeBin/readlink" <<'EOF'
  #!/bin/sh
  printf '%s\n' /nix/store/previous-system
  EOF

  cat > "$fakeBin/sudo" <<'EOF'
  #!/bin/sh
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
  exit 0
  EOF


  chmod +x "$fakeBin"/*

  run_expected_failure() {
    host="$1"
    expected="$2"
    set +e
    PATH="$fakeBin:$PATH" ${deploy}/bin/deploy "$host" > "$TMPDIR/$host.stdout" 2> "$TMPDIR/$host.stderr"
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

  run_expected_failure debord "error: Failed to activate debord"
  run_expected_failure "$localHost" "error: Failed to activate $localHost"
  if grep -q "^root@$localHost|" "$sshLog"; then
    echo "local deployment unexpectedly used SSH" >&2
    exit 1
  fi

  PATH="$fakeBin:$PATH" ${deploy}/bin/deploy addams > "$TMPDIR/addams.stdout" 2> "$TMPDIR/addams.stderr"
  grep -F "ssh://root@addams-lan" "$nixLog"
  grep -Fx "root@addams-lan|readlink -e /nix/var/nix/profiles/system" "$sshLog"
  grep -F "root@addams-lan|$fakeSystem/bin/switch-to-configuration switch" "$sshLog"
  if grep -q '^addams|' "$sshLog" || grep -q '^root@addams|' "$sshLog"; then
    echo "addams deployment used its Tailscale SSH target" >&2
    exit 1
  fi

  : > "$sshLog"
  PATH="$fakeBin:$PATH" ${deploy}/bin/deploy addams dry-activate > "$TMPDIR/preview.stdout" 2> "$TMPDIR/preview.stderr"
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

  touch "$fakeSystem/thinkpad1-tpm-deploy"
  chmod +x "$fakeSystem/thinkpad1-tpm-deploy"
  : > "$sshLog"
  run_expected_failure thinkpad1 "error: Failed TPM-aware activation of thinkpad1"
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
