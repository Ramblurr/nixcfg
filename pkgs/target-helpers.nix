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
    local selection="[ $names ]"
    if [[ $# -eq 0 ]]; then
      selection="builtins.attrNames cs"
    fi
    TARGETS_JSON=$(nix eval --json --no-write-lock-file \
      .#nixosConfigurations \
      --apply "cs: ($resolver) { configurations = cs; names = $selection; }") \
      || return 1
  }

  function target_field() {
    ${jq}/bin/jq -er --arg name "$1" --arg field "$2" \
      '.[$name][$field] | if type == "boolean" then tostring else . end' <<< "$TARGETS_JSON"
  }

  function show_help() {
    resolve_targets || return 1
    printf '%s\n' 'Usage: build <name>...' '       deploy [OPTIONS] <name,...> [ACTION]' ""
    ${jq}/bin/jq -r '
      to_entries | sort_by(.key) |
      (map(select(.value.guest)) | ([6] + map(.key | length) | max)) as $width |
      def pad: . + (" " * ($width - length));
      "Hosts",
      (.[] | select(.value.guest | not) | "  " + .key),
      "", ("Guests" | pad) + "    Runs on",
      (.[] | select(.value.guest) | "  " + (.key | pad) + "  " + .value.host)
    ' <<< "$TARGETS_JSON"
    printf '%s\n' "" \
      'Deploy actions' \
      '  switch          Activate now and make the boot default (default).' \
      '  boot            Make the boot default without activating.' \
      '  test            Activate without making the boot default.' \
      '  dry-activate    Show activation changes without activating.' \
      "" \
      '  Guests support switch only.' \
      "" \
      'Options' \
      '  build           No options; accepts one or more names.' \
      '  deploy          Passes option flags to the local Nix build.' \
      '                  Examples: --show-trace, --keep-going, --verbose' \
      '  deploy --help   Show help.' \
      "" \
      'Guest deployment builds remotely on its host.' \
      'Local build options do not apply to that remote build.' \
      'Hot switching does not apply VM hardware/kernel or host-side service/credential changes.' \
      'Those changes require a separately planned VM restart or host deployment.'
  }
''
