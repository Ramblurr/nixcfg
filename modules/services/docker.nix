{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
let
  cfg = config.modules.services.docker;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.services.docker = {
    enable = lib.mkEnableOption "";
    enableOnBoot = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
  };
  config = mkIf cfg.enable {
    environment.persistence."/persist" = {
      directories = [ "/var/lib/docker" ];
    };
    virtualisation.docker = {
      enable = true;
      enableOnBoot = cfg.enableOnBoot;
    };
  };
}
