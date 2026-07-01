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

    myhm = {
      services.nextcloud-client = {
        enable = true;
        startInBackground = true;
      };
    };
  };
}
