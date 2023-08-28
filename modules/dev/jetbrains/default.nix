{
  config,
  options,
  inputs,
  lib,
  pkgs,
  my,
  ...
}:
with lib;
with lib.my; let
  devCfg = config.modules.dev;
  cfg = devCfg.jetbrains;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.dev.jetbrains = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
    ];
    myhm = {
      home.packages = with pkgs; [
        jetbrains.idea-ultimate
        jetbrains.datagrip
        jetbrains.gateway
      ];

      persistence = mkIf withImpermanence {
        directories = [
          ".config/JetBrains"
          ".cache/JetBrains"
          ".local/share/JetBrains"
          ".java/.userPrefs/jetbrains"
        ];
      };

      home.file.".ideavimrc" = {
        source = ./configs/ideavimrc;
      };
      xdg.configFile."ideavim" = {
        source = ./configs/ideavim;
        recursive = true;
      };
    };
  };
}
