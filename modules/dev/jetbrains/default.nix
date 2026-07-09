{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  devCfg = config.modules.dev;
  cfg = devCfg.jetbrains;
in
{
  options.modules.dev.jetbrains = {
    enable = lib.mkEnableOption "";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ ];

    myhm = {
      home.packages = with pkgs; [
        jetbrains.idea
        #jetbrains.datagrip
        #jetbrains.gateway
        #jetbrains.clion
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
