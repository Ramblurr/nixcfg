#!/usr/bin/env bash
set -euo pipefail

repo_root=$(git -C "$(dirname "${BASH_SOURCE[0]}")" rev-parse --show-toplevel)
notes_home=$(mktemp -d)
trap 'rm -rf "$notes_home"' EXIT

HOME="$notes_home" \
XDG_CONFIG_HOME="$notes_home/.config" \
CODEX_HOME="$notes_home/.config/codex" \
PLANNOTATOR_DATA_DIR="$notes_home/.config/plannotator" \
PLANNOTATOR_INBOX_NOTES=1 \
  plannotator inbox --notes codex

test -s "$notes_home/.config/codex/AGENTS.md"
install -m 644 "$notes_home/.config/codex/AGENTS.md" \
  "$repo_root/modules/dev/llms/plannotator-instructions.md"
