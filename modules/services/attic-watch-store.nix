{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.attic-watch-store;
  inherit (config.modules.users.primaryUser) username;
in
{
  options.modules.services.attic-watch-store = {
    enable = lib.mkEnableOption "attic-watch-store";
  };
  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.attic-client ];
    systemd.services.attic-watch-store = {
      enable = true;
      wantedBy = [ "multi-user.target" ];
      description = "attic-watch-store";
      serviceConfig = {
        User = username;
        ExecStart = "${pkgs.attic-client}/bin/attic watch-store socozy";
        Restart = "always";
        RestartSec = 30;
      };
    };
  };
}
