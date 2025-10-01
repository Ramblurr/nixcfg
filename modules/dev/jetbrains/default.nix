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
let
  devCfg = config.modules.dev;
  cfg = devCfg.jetbrains;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.dev.jetbrains = {
    enable = lib.mkEnableOption "";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ ];

    environment.persistence."/persist" = mkIf withImpermanence {
      users.${username} = {
        directories = [
          ".config/JetBrains"
          ".cache/JetBrains"
          ".local/share/JetBrains"
          ".java/.userPrefs/jetbrains"
        ];
      };
    };
    myhm = {
      home.packages = with pkgs; [
        jetbrains.idea-ultimate
        #jetbrains.datagrip
        jetbrains.gateway
        jetbrains.clion
        jetbrains.gateway
        #java-mission-control
        #dotnet-sdk_7
        #dotnet-sdk
        #msbuild
        #mono
        #mono4
        #(with dotnetCorePackages;
        #  combinePackages [
        #    sdk_6_0
        #    sdk_7_0
        #  ])
      ];

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
