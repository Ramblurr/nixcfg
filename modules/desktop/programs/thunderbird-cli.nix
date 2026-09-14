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
  options.modules.desktop.programs.thunderbird-cli.enable = lib.mkEnableOption ''
    Thunderbird CLI, MCP server, and user bridge service.
    The Thunderbird module installs the signed AI Bridge add-on in its personal profile.
    Thunderbird must be running for email operations.
    The bridge listens on localhost ports 7700 (HTTP) and 7701 (WebSocket)
  '';

  config = lib.mkIf cfg.enable {
    myhm = {
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
