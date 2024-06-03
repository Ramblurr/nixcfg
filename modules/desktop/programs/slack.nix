{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.programs.slack;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.slack = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {

    environment.persistence."/persist" = mkIf withImpermanence {
      users.${username} = {
        directories = [ ".config/Slack" ];
      };
    };
    home-manager.users."${username}" =
      { pkgs, config, ... }@hm:
      {
        home.packages = [ pkgs.slack ];
      };
  };
}
