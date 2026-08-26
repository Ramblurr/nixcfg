{
  attic-client,
  atticCache ? "",
  coreutils,
  lib,
  nix-output-monitor,
  writeShellApplication,
}:
writeShellApplication {
  name = "cache-push";
  runtimeInputs = [
    attic-client
    coreutils
    nix-output-monitor
  ];
  text = ''
    set -euo pipefail

    die() {
      echo "error: $*" >&2
      exit 1
    }

    [[ "$#" -ge 1 ]] || die "usage: cache-push TARGET..."

    cache=${lib.escapeShellArg atticCache}
    if [[ -n "''${ATTIC_CACHE:-}" ]]; then
      cache=$ATTIC_CACHE
    fi
    [[ -n "$cache" ]] || die "ATTIC_CACHE is not set"

    installables=()
    store_paths=()
    for target in "$@"; do
      if [[ -e "$target" ]]; then
        resolved=$(realpath -e -- "$target")
        if [[ "$resolved" == /nix/store/* ]]; then
          store_paths+=("$resolved")
          continue
        fi
      fi

      if [[ "$target" == /nix/store/* ]]; then
        die "store path does not exist: $target"
      elif [[ "$target" =~ ^[a-zA-Z0-9][a-zA-Z0-9_-]*$ ]]; then
        installables+=(".#nixosConfigurations.$target.config.system.build.toplevel")
      else
        installables+=("$target")
      fi
    done

    if [[ "''${#installables[@]}" -gt 0 ]]; then
      out_paths=$(mktemp)
      trap 'rm -f "$out_paths"' EXIT
      nom build --no-link --print-out-paths --show-trace \
        "''${installables[@]}" > "$out_paths"
      while IFS= read -r path; do
        [[ "$path" == /nix/store/* ]] || die "unexpected build output: $path"
        store_paths+=("$path")
      done < "$out_paths"
    fi

    attic push --jobs 4 "$cache" "''${store_paths[@]}"
  '';
}
