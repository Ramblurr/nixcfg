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
  cfg = config.modules.desktop.programs.yubico;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.yubico = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    programs.yubikey-touch-detector.enable = true;
    myhm = {
      home.packages = with pkgs; [
        yubikey-manager
        yubioath-flutter
        age-plugin-yubikey
      ];
    };
  };
}
