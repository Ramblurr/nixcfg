{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.owncloud;
in
{
  options.modules.desktop.programs.owncloud = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {

    myhm = {
      home.packages = [ pkgs.owncloud-client ];
      services.owncloud-client.enable = true;
    };
  };
}
