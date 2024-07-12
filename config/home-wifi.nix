{
  pkgs,
  config,
  lib,
  ...
}:
let
  inherit (config.repo.secrets.global) wifi;
  cfg = config.home;
in
{
  options.home.wifi = {
    iot = {
      enable = lib.mkEnableOption "WiFi for IoT";
      interface = lib.mkOption {
        type = lib.types.str;
        default = "wlan0";
        description = "The WiFi interface to use";
      };
    };
    primary = {
      enable = lib.mkEnableOption "WiFi";
      interface = lib.mkOption {
        type = lib.types.str;
        default = "wlan0";
        description = "The WiFi interface to use";
      };
    };
  };
  config = lib.mkIf (cfg.wifi.iot.enable || cfg.wifi.primary.enable) {
    networking =
      if cfg.wifi.iot.enable then
        {
          interfaces."${cfg.wifi.iot.interface}".useDHCP = true;
          wireless = {
            interfaces = [ "${cfg.wifi.iot.interface}" ];
            enable = true;
            networks = {
              "${wifi.iot.ssid}".psk = "${wifi.iot.password}";
            };
          };
        }
      else if cfg.wifi.primary.enable then
        {
          wireless = {
            interfaces = [ "${cfg.wifi.primary.interface}" ];
            enable = true;
            networks = {
              "${wifi.primary.ssid}".psk = "${wifi.primary.password}";
            };
          };
        }
      else
        { };
  };
}
