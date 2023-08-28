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
  cfg = config.modules.desktop.programs.nheko;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.desktop.programs.nheko = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" = {
      pkgs,
      config,
      ...
    } @ hm: {
      home.packages = with pkgs; [
        nheko
      ];
      home.persistence."/persist${homeDirectory}" = mkIf withImpermanence {
        directories = [
          ".config/nheko"
          ".cache/nheko"
          ".local/share/nheko"
        ];
      };
    };
  };
}
