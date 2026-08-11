{
  cfg,
  inputs,
  lib,
  pkgs,
  settings,
}:

let
  llmAgents = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
in
{
  config = lib.mkIf cfg.enable {
    myhm =
      hm:
      let
        piSettings = settings hm.config.home.homeDirectory;
        settingsFile = pkgs.writeText "pi-settings.json" (
          builtins.toJSON (
            piSettings
            // {
              packages = map toString piSettings.packages;
            }
          )
        );
        syncSettings = pkgs.writeShellScript "sync-pi-settings" ''
          agent_dir="''${PI_CODING_AGENT_DIR:-''${XDG_CONFIG_HOME:-$HOME/.config}/pi/agent}"
          settings_file="$agent_dir/settings.json"
          mkdir -p "$agent_dir"
          tmp="$(mktemp "$agent_dir/settings.json.XXXXXX")"
          trap 'rm -f "$tmp"' EXIT

          if [ -f "$settings_file" ]; then
            ${lib.getExe pkgs.jq} -s '.[0] * .[1]' "$settings_file" ${settingsFile} > "$tmp"
          else
            printf '%s\n' '{}' | ${lib.getExe pkgs.jq} -s '.[0] * .[1]' - ${settingsFile} > "$tmp"
          fi

          chmod 0600 "$tmp"
          if [ ! -f "$settings_file" ] || ! cmp -s "$tmp" "$settings_file"; then
            mv "$tmp" "$settings_file"
          else
            rm "$tmp"
          fi
        '';
        pi = pkgs.writeShellScriptBin "pi" ''
          case "''${1-}" in
            install|remove|uninstall|update|list|config)
              exec ${lib.getExe llmAgents.pi} "$@"
              ;;
          esac

          ${syncSettings}
          exec ${lib.getExe llmAgents.pi} "$@"
        '';
      in
      {
        home.activation.piSettings = hm.lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          ${syncSettings}
        '';
        home.packages = [ pi ];
      };
  };
}
