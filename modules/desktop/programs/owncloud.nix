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
  cfg = config.modules.desktop.programs.owncloud;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.desktop.programs.owncloud = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    myhm = {
      home.packages = [
        pkgs.owncloud-client
      ];
      persistence = mkIf withImpermanence {
        directories = [
          ".config/ownCloud"
        ];
      };
    };
  };
}
