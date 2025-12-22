{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.services.docker;
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
      inherit (cfg) enableOnBoot;
    };
  };
}
