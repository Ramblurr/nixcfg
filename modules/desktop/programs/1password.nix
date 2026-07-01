{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.onepassword;
  inherit (config.modules.users.primaryUser) username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.onepassword = {
    enable = lib.mkEnableOption "";
    autostart.enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    programs._1password.enable = true;
    programs._1password-gui = {
      enable = true;
      polkitPolicyOwners = [ username ];
    };
    myhm = {
      #home.file.".config/autostart/1password.desktop" = lib.mkIf cfg.autostart.enable (
      #  lib.my.autostart "1password"
      #);
    };
    environment.etc = {
      "1password/custom_allowed_browsers" = {
        text = ''
          firefox-beta-bin
          firefox-bin
          firefox
        '';
        mode = "0755";
      };
    };

  };
}
