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
  cfg = config.modules.services.docker;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.services.docker = {
    enable = mkBoolOpt false;
    enableOnBoot = mkBoolOpt true;
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
