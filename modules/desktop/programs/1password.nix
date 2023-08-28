{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.programs.onepassword;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.desktop.programs.onepassword = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    myhm = {
      # Using flatpak 1password for now because native wayland 1password has a broken clipboard
      #users.users."${username}".packages = with pkgs; [
      #  _1password-gui
      #];
      persistence = mkIf withImpermanence {
        directories = [
          ".config/1Password"
        ];
      };
    };
  };
}
