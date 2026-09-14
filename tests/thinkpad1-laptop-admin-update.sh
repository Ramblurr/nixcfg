#!/usr/bin/env bash
# Run against the evaluated laptop-admin-update service's first ExecStart.
set -euo pipefail
updater=$(realpath "${1:?usage: $0 /nix/store/.../bin/laptop-admin-update}")
root=$(mktemp -d)
trap 'rm -rf -- "$root"' EXIT
export GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null
export GIT_AUTHOR_NAME=Test GIT_AUTHOR_EMAIL=test@example.invalid
export GIT_COMMITTER_NAME=Test GIT_COMMITTER_EMAIL=test@example.invalid
export GIT_TERMINAL_PROMPT=0

git init --quiet --bare --initial-branch=main "$root/origin"
git clone --quiet "$root/origin" "$root/publisher"
printf 'initial\n' > "$root/publisher/AGENTS.md"
git -C "$root/publisher" add AGENTS.md
git -C "$root/publisher" commit --quiet -m initial
git -C "$root/publisher" push --quiet origin main
base=$(git -C "$root/publisher" rev-parse HEAD)
printf 'updated\n' > "$root/publisher/AGENTS.md"
git -C "$root/publisher" commit --quiet -am update
git -C "$root/publisher" push --quiet origin main
latest=$(git -C "$root/publisher" rev-parse HEAD)

checkout() {
  mkdir "$root/$1"
  for repo in laptop-admin skills; do
    git clone --quiet "$root/origin" "$root/$1/$repo"
    git -C "$root/$1/$repo" reset --quiet --hard "$base"
  done
  cd "$root/$1"
}

expect_failure() {
  if "$updater"; then
    echo 'Expected updater to fail for unsafe checkout or failed fetch' >&2
    exit 1
  fi
}

checkout clean
"$updater"
"$updater"
for repo in laptop-admin skills; do
  test "$(git -C "$repo" rev-parse HEAD)" = "$latest"
  test "$(cat "$repo/AGENTS.md")" = updated
  test -z "$(git -C "$repo" status --porcelain)"
done

for target in laptop-admin skills; do
  checkout "$target-dirty"
  printf 'local edit\n' >> "$target/AGENTS.md"
  before=$(git -C "$target" diff)
  expect_failure
  test "$(git -C "$target" rev-parse HEAD)" = "$base"
  test "$(git -C "$target" diff)" = "$before"
  git -C "$target" add AGENTS.md
  before=$(git -C "$target" diff --cached)
  expect_failure
  test "$(git -C "$target" diff --cached)" = "$before"
  test "$(git -C "$target" rev-parse HEAD)" = "$base"

  checkout "$target-untracked"
  printf 'keep me\n' > "$target/notes.txt"
  expect_failure
  test "$(git -C "$target" rev-parse HEAD)" = "$base"
  test "$(cat "$target/notes.txt")" = 'keep me'

  checkout "$target-branch"
  git -C "$target" switch --quiet -c work
  expect_failure
  test "$(git -C "$target" symbolic-ref --short HEAD)" = work
  test "$(git -C "$target" rev-parse HEAD)" = "$base"
  git -C "$target" switch --quiet --detach
  expect_failure
  test "$(git -C "$target" rev-parse HEAD)" = "$base"

  checkout "$target-diverged"
  printf 'local commit\n' > "$target/local.txt"
  git -C "$target" add local.txt
  git -C "$target" commit --quiet -m local
  local_head=$(git -C "$target" rev-parse HEAD)
  expect_failure
  test "$(git -C "$target" rev-parse HEAD)" = "$local_head"
  test -z "$(git -C "$target" status --porcelain)"

  checkout "$target-ahead"
  git -C "$target" merge --quiet --ff-only origin/main
  printf 'local commit\n' > "$target/local.txt"
  git -C "$target" add local.txt
  git -C "$target" commit --quiet -m local
  local_head=$(git -C "$target" rev-parse HEAD)
  expect_failure
  test "$(git -C "$target" rev-parse HEAD)" = "$local_head"

  checkout "$target-unavailable"
  git -C "$target" remote set-url origin "$root/nonexistent"
  expect_failure
  test "$(git -C "$target" rev-parse HEAD)" = "$base"
done
printf 'PASS: both repositories fast-forward; dirty/staged/untracked, branch/detached, divergence/local-ahead and fetch errors fail without discarding work\n'
