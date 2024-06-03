{
  config,
  lib,
  pkgs,
  ...
}:
{
  #systemd.services.systemd-networkd.serviceConfig.Environment = ["SYSTEMD_LOG_LEVEL=debug"];
  systemd.network = {
    netdevs = {
      "20-vlmgmt" = {
        netdevConfig = {
          Kind = "vlan";
          Name = "vlprim4";
        };
        vlanConfig = {
          Id = 4;
        };
      };
      "20-vlmgmt9" = {
        netdevConfig = {
          Kind = "vlan";
          Name = "vlmgmt9";
        };

        vlanConfig = {
          Id = 9;
        };
      };
    };
    networks = {
      "10-eno1" = {
        matchConfig.Name = "eno1";
        vlan = [
          "vlmgmt9"
          "vlprim4"
        ];
      };

      "30-vlprim4" = {
        matchConfig.Name = "vlprim4";
        linkConfig.RequiredForOnline = "routable";
        networkConfig = {
          DHCP = "yes";
          #IPForward = "yes";
          DNSSEC = "no";
        };

        domains = config.repo.secrets.local.dns.domains;
        dhcpV4Config.Use6RD = "yes";
        dhcpV4Config.RouteMetric = 512;
        routes = [
          {
            #routeConfig = {
            Destination = "192.168.8.0/22";
            Gateway = "10.9.4.27";
            #};
          }
        ];
      };
      "40-vlmgmt9" = {
        matchConfig.Name = "vlmgmt9";
        addresses = map (addr: { Address = addr; }) [ "10.9.8.33/23" ];
        networkConfig = {
          DHCP = "no";
          #IPForward = "yes";
          DNSSEC = "no";
        };
        domains = config.repo.secrets.local.dns.domains;
      };
    };
  };
}
