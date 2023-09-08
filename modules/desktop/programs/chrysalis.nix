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
  cfg = config.modules.desktop.programs.chrysalis;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.desktop.programs.chrysalis = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    services.udev.packages = [edge.chrysalis];
    home-manager.users."${username}" = {
      pkgs,
      config,
      ...
    } @ hm: {
      home.packages = [
        # It is a statically linked AppImage, so we always want the latest
        edge.chrysalis
      ];
      home.persistence."/persist${homeDirectory}" = mkIf withImpermanence {
        directories = [
          ".config/chrysalis"
        ];
      };
    };
  };
}
