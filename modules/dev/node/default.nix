{ config, options, lib, pkgs, my, ... }:
with lib;
with lib.my;
let
  devCfg = config.modules.dev;
  cfg = devCfg.node;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.dev.node = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ ];
    home-manager.users."${username}" = {
      home.packages = with pkgs; [ nodejs_20 ];

      xdg.configFile."npm" = {
        source = ./configs/npm;
        recursive = true;
      };
      home.persistence."/persist${homeDirectory}" = mkIf withImpermanence {
        directories = [ ".config/npm" ".cache/npm-packages" ".local/share/npm" ];
      };
    };
  };
}
