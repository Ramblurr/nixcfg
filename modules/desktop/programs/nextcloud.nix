{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.nextcloud;
  inherit (config.modules.users.primaryUser) username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.nextcloud = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {

    environment.persistence."/persist" = mkIf withImpermanence {
      users.${username} = {
        directories = [
          ".config/Nextcloud"
        ];
      };
    };
    myhm = {
      services.nextcloud-client = {
        enable = true;
        startInBackground = true;
      };
    };
  };
}
