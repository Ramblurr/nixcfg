{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.desktop.programs.nextcloud;
  inherit (config.modules.users.primaryUser) username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.nextcloud = {
    enable = lib.mkEnableOption "";
  };
  config = lib.mkIf cfg.enable {

    environment.persistence."/persist" = lib.mkIf withImpermanence {
      users.${username} = {
        directories = [
          ".cache/Nextcloud"
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
