{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.onepassword;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.onepassword = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    programs._1password = {
      enable = true;
    };
    programs._1password-gui = {
      enable = true;
      polkitPolicyOwners = [ username ];
    };

    environment.persistence."/persist" = mkIf withImpermanence {
      users.${username} = {
        directories = [ ".config/1Password" ];
      };
    };
  };
}
