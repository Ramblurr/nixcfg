{
  writeShellApplication,
  jq,
  lib,
}:
writeShellApplication {
  name = "build";
  text = ''
    set -euo pipefail
    [[ "$#" -ge 1 ]] \
      || { echo "usage: build <HOST|GUEST>... (run from nixcfg-private)" >&2; exit 1; }
    cd "$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
    ${import ./target-helpers.nix { inherit jq lib; }}
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
