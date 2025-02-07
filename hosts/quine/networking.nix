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
      "20-vlprim4" = {
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
      "30-brprim4" = {
        netdevConfig = {
          Name = "brprim4";
          Kind = "bridge";
          MTUBytes = "1500";
        };
      };

      "20-vlvpn70" = {
        netdevConfig = {
          Kind = "vlan";
          Name = "vlvpn70";
        };
        vlanConfig = {
          Id = 70;
        };
      };
      "30-brvpn70" = {
        netdevConfig = {
          Name = "brvpn70";
          Kind = "bridge";
          MTUBytes = "1500";
        };
      };
      "30-brwork" = {
        netdevConfig = {
          Name = "brwork";
          Kind = "bridge";
          MTUBytes = "1500";
        };
      };

      #"30-virbr0" = {
      #  netdevConfig = {
      #    Kind = "bridge";
      #    Name = "virbr0";
      #  };
      #};
    };
    networks = {
      "10-eno2" = {
        matchConfig.Name = "eno2";
        linkConfig = {
          Unmanaged = "yes";
          ActivationPolicy = "down";
        };
      };
      "10-wifi" = {
        # This is the builtin wifi
        matchConfig.Name = "wlp8s0";
        linkConfig = {
          Unmanaged = "yes";
          ActivationPolicy = "down";
        };
      };
      "10-eno1" = {
        matchConfig.Name = "eno1";
        vlan = [
          "vlmgmt9"
          "vlprim4"
          "vlvpn70"
        ];
        networkConfig.LinkLocalAddressing = "no";
        linkConfig.RequiredForOnline = "carrier";
      };

      "20-vlprim4" = {
        matchConfig = {
          Name = [
            "vlprim4"
            "vm-*"
          ];
        };
        linkConfig.RequiredForOnline = "carrier";
        networkConfig = {
          Bridge = "brprim4";
        };
      };
      "30-brprim4" = {
        matchConfig.Name = "brprim4";
        linkConfig.RequiredForOnline = "routable";
        networkConfig = {
          DHCP = "yes";
        };

        domains = config.repo.secrets.local.dns.domains;
        dhcpV4Config.Use6RD = "yes";
        dhcpV4Config.RouteMetric = 512;
        routes = [
          {
            Destination = "192.168.8.0/22";
            Gateway = "10.9.4.21";
            GatewayOnLink = true;
          }
        ];
      };
      "40-vlmgmt9" = {
        matchConfig.Name = "vlmgmt9";
        addresses = map (addr: { Address = addr; }) [ "10.9.8.33/23" ];
        linkConfig.RequiredForOnline = "routable";
        networkConfig = {
          DHCP = "no";
        };
        domains = config.repo.secrets.local.dns.domains;
      };

      "20-vlvpn70" = {
        matchConfig.Name = "vlvpn70";
        linkConfig.RequiredForOnline = "carrier";
        networkConfig = {
          Bridge = "brvpn70";
        };
      };

      "30-brvpn70" = {
        matchConfig.Name = "brvpn70";
        linkConfig.RequiredForOnline = "carrier";
        networkConfig = {
          LinkLocalAddressing = "no";
        };
      };
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
            dhcpServerStaticLeaseConfig = {
              Address = config.repo.secrets.local.workIp;
              MACAddress = config.repo.secrets.local.workMac;
            };
          }
        ];

        dhcpServerConfig = {
          PoolOffset = 100;
          PoolSize = 50;
          EmitDNS = "no";
          EmitRouter = "no";
        };
      };

      #"4-virbr0" = {
      #  matchConfig.Name = "virbr0";
      #  enable = true;
      #  address = [
      #    "10.0.3.20/24"
      #    "2001:470:f026:103::20/64"
      #  ];
      #  routes = [
      #    { Gateway = "10.0.3.1"; }
      #    { Gateway = "2001:470:f026:103::1"; }
      #  ];
      #};
    };
  };
}
