{
  deploy,
  pkgs,
}:
pkgs.runCommand "deploy-propagates-activation-failures" { } ''
  fakeBin="$TMPDIR/fake-bin"
  export workRoot="$TMPDIR/work"
  export fakeSystem="$TMPDIR/fake-system"
  mkdir -p "$fakeBin" "$workRoot" "$fakeSystem/bin"
  touch "$workRoot/flake.nix"

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
  if [ "$1" = "copy" ]; then
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

  if [ "$host" = "debord" ] && [ "$1" = "readlink" ]; then
    printf '%s\n' /nix/store/previous-system
    exit 0
  fi

  if [ "$host" = "root@debord" ]; then
    case "$1" in
      /run/current-system/sw/bin/nix-env)
        exit 0
        ;;
      "$fakeSystem/bin/switch-to-configuration")
        exit 42
        ;;
      nvd)
        exit 0
        ;;
    esac
  fi

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

  run_failure_test() {
    host="$1"
    set +e
    PATH="$fakeBin:$PATH" ${deploy}/bin/deploy "$host" > "$TMPDIR/$host.stdout" 2> "$TMPDIR/$host.stderr"
    status=$?
    set -e

    if [ "$status" -eq 0 ]; then
      cat "$TMPDIR/$host.stderr" >&2
      echo "deploy returned success after activation failed for $host" >&2
      exit 1
    fi

    grep -F "error: Failed to activate $host" "$TMPDIR/$host.stderr"
  }

  run_failure_test debord
  run_failure_test quine
  touch "$out"
''
