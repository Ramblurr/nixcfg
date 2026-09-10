{
  writeShellApplication,
  jq,
  lib,
}:
writeShellApplication {
  name = "build";
  text = ''
    set -euo pipefail
    cd "$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
    ${import ./target-helpers.nix { inherit jq lib; }}
    if [[ $# -eq 0 ]]; then
      show_help
      exit 0
    fi
    resolve_targets "$@"
    targets=()
    for name in "$@"; do
      targets+=(".#$(target_field "$name" buildAttribute)")
    done
    if command -v nom &>/dev/null; then
      builder=nom
    else
      builder=nix
    fi
    "$builder" build --no-link --print-out-paths --show-trace "''${targets[@]}"
  '';
}
