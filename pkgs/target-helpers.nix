# Shell helpers shared by build and deploy; metadata is evaluated at invocation time.
{ jq, lib }:
''
  function resolve_targets() {
    local name names=""
    # This is Nix source, not shell interpolation; inline it to keep evaluation pure.
    # shellcheck disable=SC2016
    local resolver=${lib.escapeShellArg (builtins.readFile ./resolve-targets.nix)}
    for name in "$@"; do
      [[ "$name" =~ ^[a-zA-Z0-9][a-zA-Z0-9_-]*$ ]] \
        || { echo "error: invalid target name: $name" >&2; return 1; }
      names+="\"$name\" "
    done
    TARGETS_JSON=$(nix eval --json --no-write-lock-file \
      .#nixosConfigurations \
      --apply "cs: ($resolver) { configurations = cs; names = [ $names ]; }") \
      || return 1
  }

  function target_field() {
    ${jq}/bin/jq -er --arg name "$1" --arg field "$2" \
      '.[$name][$field] | if type == "boolean" then tostring else . end' <<< "$TARGETS_JSON"
  }
''
