{
  pkgs,
  config,
  lib,
  ...
}:
with builtins;
with lib;
with lib.my; let
  creds = builtins.readFile ../secrets/wifi-iot.secrets;
  lines = builtins.split "\n" creds;
  ssid = builtins.elemAt lines 0;
  password = builtins.elemAt lines 2;
  cfg = config.home;
in {
  options.home.wifi = {
    iot = {
      enable = mkEnableOption "WiFi for IoT";
      interface = mkStrOpt "wlan0";
    };
  };
  config = {
    time.timeZone = lib.mkDefault "Europe/Berlin";
    i18n.defaultLocale = mkDefault "en_US.utf8";

    networking = mkIf cfg.wifi.iot.enable {
      interfaces."${cfg.wifi.iot.interface}".useDHCP = true;
      wireless = {
        interfaces = ["${cfg.wifi.iot.interface}"];
        enable = true;
        networks = {
          "${ssid}".psk = "${password}";
        };
      };
    };
  };
}
