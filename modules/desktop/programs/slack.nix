{
  options,
  config,
  lib,
  pkgs,
  edge,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.programs.slack;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.desktop.programs.slack = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" = {
      pkgs,
      config,
      ...
    } @ hm: {
      home.packages = [
        edge.slack
      ];
      home.persistence."/persist${homeDirectory}" = mkIf withImpermanence {
        directories = [
          ".config/slack"
        ];
      };
    };
  };
}
