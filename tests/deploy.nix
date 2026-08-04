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
  [ "$1" = "copy" ] && exit 0
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
    debord:readlink|addams-lan:readlink)
      printf '%s\n' /nix/store/previous-system
      exit 0
      ;;
  esac

  case "$host:$1" in
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

    grep -F "$expected" "$TMPDIR/$host.stderr"
  }

  run_expected_failure debord "error: Failed to activate debord"
  run_expected_failure quine "error: Failed to activate quine"

  PATH="$fakeBin:$PATH" ${deploy}/bin/deploy addams > "$TMPDIR/addams.stdout" 2> "$TMPDIR/addams.stderr"
  grep -F "ssh://root@addams-lan" "$nixLog"
  grep -F "addams-lan|readlink -e /nix/var/nix/profiles/system" "$sshLog"
  grep -F "root@addams-lan|$fakeSystem/bin/switch-to-configuration switch" "$sshLog"
  if grep -q '^addams|' "$sshLog" || grep -q '^root@addams|' "$sshLog"; then
    echo "addams deployment used its Tailscale SSH target" >&2
    exit 1
  fi

  touch "$out"
''
