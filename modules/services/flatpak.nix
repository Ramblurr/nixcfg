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
  cfg = config.modules.services.flatpak;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.services.flatpak = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    services.flatpak.enable = true;
    environment.persistence."/persist" = {
      directories = [
        "/var/lib/flatpak"
      ];
    };
    home-manager.users."${username}" = {
      home.persistence."/persist${homeDirectory}" = mkIf withImpermanence {
        directories = [
          ".cache/flatpak"
          ".local/share/flatpak"
          ".var/app"
        ];
      };
    };
  };
}
