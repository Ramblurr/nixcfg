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
  cfg = config.modules.desktop.programs.morgen;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.morgen = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    environment.persistence."/persist" = lib.mkIf withImpermanence {
      users.${username} = {
        directories = [
          ".config/Morgen"
        ];
      };
    };
    myhm = {
      home.packages = with pkgs; [
        morgen
      ];
    };
  };
}
