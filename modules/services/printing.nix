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
  cfg = config.modules.services.printing;
in
{
  options.modules.services.printing = {
    enable = mkBoolOpt false;
    drivers = mkOption {
      type = types.listOf types.package;
      default = [ ];
    };
  };
  config = mkIf cfg.enable {
    services.printing.enable = true;
    services.printing.drivers = cfg.drivers;
    services.avahi.enable = mkDefault true;
    services.avahi.nssmdns4 = mkDefault true;
    # for a WiFi printer
    services.avahi.openFirewall = mkDefault true;
  };
}
