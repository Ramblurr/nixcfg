{
  config,
  lib,
  ...
}:
{
  inherit (config.repo.secrets.site) site;
  #systemd.services.systemd-networkd.serviceConfig.Environment = ["SYSTEMD_LOG_LEVEL=debug"];
  services.resolved.settings.Resolve.FallbackDNS = [
    "1.1.1.1#cloudflare-dns.com"
    "8.8.8.8#dns.google"
    "1.0.0.1#cloudflare-dns.com"
    "8.8.4.4#dns.google"
  ];
  systemd.network = {
    config.networkConfig = {
      IPv4Forwarding = true;
      #IPv6Forwarding = false;
    };
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
        "30-prim" = {
          dhcpV4Config = {
            UseRoutes = lib.mkForce true;
            UseDNS = lib.mkForce true;
          };
          dhcpV6Config = {
            UseDNS = lib.mkForce false;
          };
          networkConfig = {
            IPv6AcceptRA = lib.mkForce false;
            LinkLocalAddressing = lib.mkForce false;
          };
        };
        "30-vpn" = {
          dhcpV4Config = {
            UseRoutes = lib.mkForce false;
            UseDNS = lib.mkForce false;
          };
          dhcpV6Config = {
            UseDNS = lib.mkForce false;
          };
          networkConfig = {
            IPv6AcceptRA = lib.mkForce false;
            LinkLocalAddressing = lib.mkForce false;
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
