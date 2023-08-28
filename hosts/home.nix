{
  pkgs,
  config,
  lib,
  ...
}:
with builtins;
with lib; {
  time.timeZone = lib.mkDefault "Europe/Berlin";
  i18n.defaultLocale = mkDefault "en_US.utf8";

  systemd.network.networks."20-local-routes" = {
    matchConfig.Name = "eno1";
    linkConfig.RequiredForOnline = "routable";

    networkConfig = {
      DHCP = "yes";
      IPForward = "yes";
      DNSSEC = "no";
    };

    dhcpV4Config.Use6RD = "yes";
    dhcpV4Config.RouteMetric = 512;
    domains = lib.pipe ../secrets/resolved-domain-secret.secrets [
      builtins.readFile
      (lib.splitString "\n")
      (lib.filter (x: x != ""))
    ];
    routes = [
      {
        routeConfig = {
          Gateway = "192.168.1.1";
          Metric = 2000;
        };
      }
    ];
  };
}
