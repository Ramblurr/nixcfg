{
  config,
  lib,
  pkgs,
  ...
}:
{
  #systemd.services.systemd-networkd.serviceConfig.Environment = ["SYSTEMD_LOG_LEVEL=debug"];
  systemd.network = {
    links = {
      "10-lan0" = {
        matchConfig.MACAddress = config.repo.secrets.local.lan0.mac;
        linkConfig.Name = "lan0";
      };
      "10-lan1" = {
        matchConfig.MACAddress = config.repo.secrets.local.lan1.mac;
        linkConfig.Name = "lan1";
      };
    };
    netdevs = {
      "30-brwork" = {
        netdevConfig = {
          Name = "brwork";
          Kind = "bridge";
          MTUBytes = "1500";
        };
      };
    };
    networks =
      # Disabled interfaces
      (lib.mori.merge (
        map
          (iface: {
            "10-${iface}" = {
              matchConfig.Name = iface;
              networkConfig.ConfigureWithoutCarrier = true;
              linkConfig = {
                Unmanaged = "yes";
                ActivationPolicy = "always-down";
              };
            };
          })
          [
            "lan1"
            "wlp8s0"
          ]
      ))
      // {
        #"30-brprim4" = {
        #  matchConfig.Name = "brprim4";
        #  linkConfig.RequiredForOnline = "routable";
        #  networkConfig = {
        #    DHCP = "yes";
        #    IPv6AcceptRA = true;
        #  };

        #  domains = config.repo.secrets.local.dns.domains;
        #  dhcpV4Config.Use6RD = "yes";
        #  dhcpV4Config.RouteMetric = 512;
        #  routes = [
        #    {
        #      Destination = "192.168.8.0/22";
        #      Gateway = "10.9.4.21";
        #      GatewayOnLink = true;
        #    }
        #  ];
        #};
        "30-brwork" = {
          matchConfig.Name = "brwork";
          linkConfig.RequiredForOnline = "no";
          addresses = [
            config.repo.secrets.local.workBridgeAddress
          ];
          networkConfig = {
            ConfigureWithoutCarrier = true;
            DHCPServer = "yes";
          };

          dhcpServerStaticLeases = [
            {
              Address = config.repo.secrets.local.workIp;
              MACAddress = config.repo.secrets.local.workMac;
            }
          ];

          dhcpServerConfig = {
            PoolOffset = 100;
            PoolSize = 50;
            EmitDNS = "no";
            EmitRouter = "no";
          };
        };
      };
  };
}
