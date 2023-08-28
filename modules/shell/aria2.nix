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
  cfg = config.modules.shell.aria2;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.shell.aria2 = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" = {
      programs.aria2 = {
        enable = true;
        settings = {
          listen-port = "6881-6999";
          dht-listen-port = "6881-6999";
        };
      };
    };
  };
}
