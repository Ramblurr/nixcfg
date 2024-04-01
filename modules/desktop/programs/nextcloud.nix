{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.programs.nextcloud;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.nextcloud = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    myhm = {
      services.nextcloud-client = {
        enable = false;
        startInBackground = true;
      };
      persistence = mkIf withImpermanence {
        directories = [
          ".config/Nextcloud"
          ".local/share/Nextcloud"
        ];
      };
    };
  };
}
