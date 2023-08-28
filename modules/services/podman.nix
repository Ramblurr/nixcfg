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
  cfg = config.modules.services.podman;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.services.podman = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    environment.persistence."/persist" = {
      directories = [
        "/var/lib/containers"
      ];
    };
    virtualisation.podman = {
      enable = true;
      defaultNetwork.settings.dns_enabled = true;
      extraPackages = [pkgs.zfs];
    };
  };
}
