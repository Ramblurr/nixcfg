{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.desktop.programs.thunderbird-cli;
in
{
  options.modules.desktop.programs.thunderbird-cli = {
    enable = lib.mkEnableOption ''
      Thunderbird CLI, MCP server, and user bridge service.
      Thunderbird must be running for email operations.
      The bridge listens on localhost ports 7700 (HTTP) and 7701 (WebSocket)
    '';
    flatpak.profile = lib.mkOption {
      type = lib.types.nullOr (lib.types.strMatching "[a-zA-Z0-9_-][a-zA-Z0-9._-]*");
      default = null;
      example = "i1noqonc.default-release";
      description = ''
        Existing profile directory under ~/.var/app/org.mozilla.thunderbird/.thunderbird.
        When set, copy the AI Bridge add-on there and enable sideloaded extensions
        through user.js. Restart Thunderbird after activation.
        Other add-ons and preferences are preserved. The copied add-on and preference
        remain when this option is disabled; remove them through the profile or UI.
        Null uses the native Thunderbird module's personal profile instead.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.flatpak.profile == null || !config.modules.desktop.programs.thunderbird.enable;
        message = "Use either native Thunderbird or thunderbird-cli.flatpak.profile, not both.";
      }
    ];

    myhm = hm: {
      home.activation.thunderbirdFlatpakBridge = lib.mkIf (cfg.flatpak.profile != null) (
        hm.lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          profile=${lib.escapeShellArg "${hm.config.home.homeDirectory}/.var/app/org.mozilla.thunderbird/.thunderbird/${cfg.flatpak.profile}"}
          if [[ ! -d "$profile" || -L "$profile/user.js" ]]; then
            echo "Thunderbird Flatpak profile must exist and user.js must not be a symlink: $profile" >&2
            exit 1
          fi

          # Copy, rather than symlink: Flatpak cannot resolve arbitrary store paths.
          source=${pkgs.thunderbird-ai-bridge}/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/thunderbird-ai@extension.xpi
          target="$profile/extensions/thunderbird-ai@extension.xpi"
          if ! ${pkgs.diffutils}/bin/cmp -s "$source" "$target"; then
            run ${pkgs.coreutils}/bin/install -Dm644 "$source" "$target.new"
            run ${pkgs.coreutils}/bin/mv -f "$target.new" "$target"
          fi

          setting='user_pref("extensions.autoDisableScopes", 0);'
          if [[ "$(${pkgs.coreutils}/bin/tail -n 1 "$profile/user.js" 2>/dev/null)" != "$setting" ]]; then
            run ${pkgs.bash}/bin/bash -c 'printf "\\n%s\\n" "$1" >> "$2"' -- "$setting" "$profile/user.js"
          fi
        ''
      );
      home.packages = [
        # The npm workspaces share conflicting lib/node_modules paths.
        (pkgs.buildEnv {
          name = "thunderbird-cli-tools";
          paths = [
            pkgs.thunderbird-cli
            pkgs.thunderbird-cli-bridge
            pkgs.thunderbird-cli-mcp
          ];
          pathsToLink = [ "/bin" ];
        })
      ];

      systemd.user.services.tb-bridge = {
        Unit = {
          Description = "Thunderbird CLI bridge";
          After = [ "graphical-session.target" ];
          PartOf = [ "graphical-session.target" ];
        };
        Service = {
          ExecStart = "${pkgs.thunderbird-cli-bridge}/bin/tb-bridge";
          Restart = "on-failure";
          RestartSec = 5;
        };
        Install.WantedBy = [ "graphical-session.target" ];
      };
    };
  };
}
