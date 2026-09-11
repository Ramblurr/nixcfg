{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.opencloud;
in
{
  options.modules.desktop.programs.opencloud = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {

    myhm = {
      home.packages = [
        pkgs.opencloud-desktop
      ]
      ++ lib.optional config.modules.desktop.kde.enable pkgs.desktop-shell-integration-dolphin;
      systemd.user.services.opencloud-desktop = {
        Unit = {
          Description = "OpenCloud Desktop Client";
          After = [ "graphical-session.target" ];
          PartOf = [ "graphical-session.target" ];
        };

        Service = {
          ExecStart = "${pkgs.opencloud-desktop}/bin/opencloud";
          Restart = "on-failure";
          RestartSec = 5;
          Slice = "app-graphical.slice";
        };

        Install = {
          WantedBy = [ "graphical-session.target" ];
        };
      };
    };
  };
}
