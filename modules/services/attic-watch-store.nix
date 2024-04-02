{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.services.attic-watch-store;
  username = config.modules.users.primaryUser.username;
  package = inputs.attic.packages.${pkgs.system}.attic-client;
in
{
  options.modules.services.attic-watch-store = {
    enable = lib.mkEnableOption "attic-watch-store";
  };
  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ package ];
    systemd.services.attic-watch-store = {
      enable = true;
      wantedBy = [ "multi-user.target" ];
      description = "attic-watch-store";
      serviceConfig = {
        User = username;
        ExecStart = "${package}/bin/attic watch-store socozy";
        Restart = "always";
        RestartSec = 30;
      };
    };
  };
}
