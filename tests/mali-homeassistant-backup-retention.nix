{ pkgs }:
let
  prune = import ../scripts/homeassistant-backup-retention { inherit pkgs; };
in
pkgs.runCommand "mali-homeassistant-backup-retention-test" { } ''
  # stdenv exports TZ=UTC, so the local-time case needs an explicit zone.
  export TZ=Europe/Berlin TZDIR=${pkgs.tzdata}/share/zoneinfo
  dir=$PWD/backups
  mkdir "$dir"
  # Home Assistant tarballs hold homeassistant.tar.gz and end with
  # ./backup.json. Add-on update backups have no core config, so their
  # "homeassistant" key is null.
  mkdir meta
  head -c 65536 /dev/urandom > meta/homeassistant.tar.gz
  tarball() {
    echo "{\"homeassistant\": $3}" > meta/backup.json
    tar -cf "$dir/$1.tar" -C meta ./homeassistant.tar.gz ./backup.json
    touch -d "$2" "$dir/$1.tar"
  }
  backup() { tarball "$1" "$2" '{"version": "2026.10.0"}'; }
  addon_backup() { tarball "$1" "$2" null; }

  # Local times. Now is 2026-11-26 12:00.
  backup recent "2026-11-26 11:30"
  backup 2026-11-26-1000 "2026-11-26 10:00"
  backup 2026-11-26-0600 "2026-11-26 06:00"
  backup 2026-11-25-2000 "2026-11-25 20:00"
  backup 2026-11-25-0800 "2026-11-25 08:00"
  addon_backup addon-2026-11-25-2200 "2026-11-25 22:00"
  # 23:30 UTC on the 23rd, so this is only its own day in local time.
  backup 2026-11-24-0030 "2026-11-24 00:30"
  backup 2026-11-23-2200 "2026-11-23 22:00"
  backup 2026-11-21-1200 "2026-11-21 12:00"
  backup 2026-11-20-1200 "2026-11-20 12:00"
  backup 2026-11-19-1200 "2026-11-19 12:00"
  backup 2026-11-18-1200 "2026-11-18 12:00"
  backup 2026-11-14-1200 "2026-11-14 12:00"
  # A truncated upload, newer than 11-14 in the same ISO week. It must
  # neither be deleted nor push 11-14 out.
  backup 2026-11-15-1200-truncated "2026-11-15 12:00"
  truncate -s 20000 "$dir/2026-11-15-1200-truncated.tar"
  touch -d "2026-11-15 12:00" "$dir/2026-11-15-1200-truncated.tar"
  # Newer than 11-14 12:00 in the same day and week, with backup.json and
  # core intact but cut exactly at the header of a later member, so tar
  # sees no error. It must not displace 11-14 12:00 either.
  head -c 4096 /dev/urandom > meta/addon.tar.gz
  echo '{"homeassistant": {"version": "2026.10.0"}}' > meta/backup.json
  tar -b 1 -cf head.tar -C meta ./homeassistant.tar.gz ./backup.json
  tar -b 1 -cf "$dir/2026-11-14-1800-cut.tar" -C meta ./homeassistant.tar.gz ./backup.json ./addon.tar.gz
  truncate -s $(( $(stat -c %s head.tar) - 1024 )) "$dir/2026-11-14-1800-cut.tar"
  touch -d "2026-11-14 18:00" "$dir/2026-11-14-1800-cut.tar"
  # Sunday: ISO week 45 with 11-03, so it replaces 11-03 as that week's
  # keeper. Sunday-start weeks would keep 11-03 instead.
  backup 2026-11-08-1200 "2026-11-08 12:00"
  backup 2026-11-10-1200 "2026-11-10 12:00"
  backup 2026-11-03-1200 "2026-11-03 12:00"
  backup 2026-11-01-1200 "2026-11-01 12:00"
  backup 2026-10-31-1200 "2026-10-31 12:00"
  backup 2026-10-05-1200 "2026-10-05 12:00"
  for month in 2025-0{3..9} 2025-1{0..2} 2026-0{1..9}; do
    backup "$month-15-1200" "$month-15 12:00"
  done
  backup 2024-06-15-1200 "2024-06-15 12:00"
  backup 2024-02-01-1200 "2024-02-01 12:00"
  backup 2023-01-01-1200 "2023-01-01 12:00"
  touch -d "2020-01-01" "$dir/notes.txt" "$dir/empty.tar"
  mkdir "$dir/nested.tar"
  touch -d "2020-01-01" "$dir/nested.tar"
  ln -s notes.txt "$dir/link.tar"

  expected=$(sort <<'EOF'
  recent.tar
  2026-11-26-1000.tar
  2026-11-25-2000.tar
  addon-2026-11-25-2200.tar
  2026-11-24-0030.tar
  2026-11-23-2200.tar
  2026-11-21-1200.tar
  2026-11-20-1200.tar
  2026-11-19-1200.tar
  2026-11-14-1200.tar
  2026-11-15-1200-truncated.tar
  2026-11-14-1800-cut.tar
  2026-11-08-1200.tar
  2026-10-31-1200.tar
  2026-09-15-1200.tar
  2026-08-15-1200.tar
  2026-07-15-1200.tar
  2026-06-15-1200.tar
  2026-05-15-1200.tar
  2026-04-15-1200.tar
  2026-03-15-1200.tar
  2026-02-15-1200.tar
  2026-01-15-1200.tar
  2025-12-15-1200.tar
  2024-06-15-1200.tar
  2023-01-01-1200.tar
  notes.txt
  empty.tar
  nested.tar
  link.tar
  EOF
  )

  run() { ${pkgs.lib.getExe prune} "$dir" --now 2026-11-26T12:00:00 "$@"; }
  before=$(ls -A "$dir" | sort)

  run --dry-run
  if [ "$(ls -A "$dir" | sort)" != "$before" ]; then
    echo "dry run changed the directory" >&2
    exit 1
  fi

  run
  actual=$(ls -A "$dir" | sort)
  if [ "$actual" != "$expected" ]; then
    diff <(echo "$expected") <(echo "$actual") >&2
    exit 1
  fi

  second=$(run)
  echo "$second"
  if grep -q '^delete ' <<<"$second" || [ "$(ls -A "$dir" | sort)" != "$expected" ]; then
    echo "second run deleted files" >&2
    exit 1
  fi
  touch "$out"
''
