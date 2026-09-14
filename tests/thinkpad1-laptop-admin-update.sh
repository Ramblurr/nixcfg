#!/usr/bin/env bash
# Run against the evaluated laptop-admin-update service executable.
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
  git clone --quiet "$root/origin" "$root/$1"
  git -C "$root/$1" reset --quiet --hard "$base"
  cd "$root/$1"
}

checkout clean
"$updater"
test "$(git rev-parse HEAD)" = "$latest"
test "$(cat AGENTS.md)" = updated
"$updater"
test -z "$(git status --porcelain)"

checkout dirty
printf 'local edit\n' >> AGENTS.md
before=$(git diff)
"$updater"
test "$(git rev-parse HEAD)" = "$base"
test "$(git diff)" = "$before"
git add AGENTS.md
before=$(git diff --cached)
"$updater"
test "$(git diff --cached)" = "$before"
test "$(git rev-parse HEAD)" = "$base"

checkout untracked
printf 'keep me\n' > notes.txt
"$updater"
test "$(git rev-parse HEAD)" = "$base"
test "$(cat notes.txt)" = 'keep me'

checkout branch
git switch --quiet -c work
"$updater"
test "$(git symbolic-ref --short HEAD)" = work
test "$(git rev-parse HEAD)" = "$base"
git switch --quiet --detach
"$updater"
test "$(git rev-parse HEAD)" = "$base"

checkout diverged
printf 'local commit\n' > local.txt
git add local.txt
git commit --quiet -m local
local_head=$(git rev-parse HEAD)
"$updater"
test "$(git rev-parse HEAD)" = "$local_head"
test -z "$(git status --porcelain)"

checkout ahead
git merge --quiet --ff-only origin/main
printf 'local commit\n' > local.txt
git add local.txt
git commit --quiet -m local
local_head=$(git rev-parse HEAD)
"$updater"
test "$(git rev-parse HEAD)" = "$local_head"

checkout unavailable
git remote set-url origin "$root/nonexistent"
if "$updater"; then
  echo 'Expected fetch failure to remain visible to systemd' >&2
  exit 1
fi
test "$(git rev-parse HEAD)" = "$base"
printf 'PASS: fast-forward, repeat run, unstaged/staged/untracked changes, branch, detached HEAD, divergence, local-ahead, and fetch failure\n'
