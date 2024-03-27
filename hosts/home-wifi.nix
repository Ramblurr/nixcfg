{ pkgs, config, lib, ... }:
with builtins;
with lib;
with lib.my;
let cfg = config.home;
in {
  options.home.wifi = {
    iot = {
      enable = mkEnableOption "WiFi for IoT";
      interface = mkStrOpt "wlan0";
      default = false;
    };

    primary = {
      enable = mkEnableOption "WiFi";
      interface = mkStrOpt "wlan0";
      default = false;
    };
  };
  config = {
    networking = if cfg.wifi.iot.enable then {
      interfaces."${cfg.wifi.iot.interface}".useDHCP = true;
      wireless = let
        creds = builtins.readFile ../secrets/wifi-iot.secrets;
        lines = builtins.split "\n" creds;
        ssid = builtins.elemAt lines 0;
        password = builtins.elemAt lines 2;
      in {
        interfaces = [ "${cfg.wifi.iot.interface}" ];
        enable = true;
        networks = { "${ssid}".psk = "${password}"; };
      };
    } else if cfg.wifi.primary.enable then {
      wireless = let
        creds = builtins.readFile ../secrets/wifi.secrets;
        lines = builtins.split "\n" creds;
        ssid = builtins.elemAt lines 0;
        password = builtins.elemAt lines 2;
      in {
        interfaces = [ "${cfg.wifi.primary.interface}" ];
        enable = true;
        networks = { "${ssid}".psk = "${password}"; };
      };
    } else
      { };
  };
}
