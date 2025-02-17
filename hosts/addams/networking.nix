{
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (lib.mori) keys mapcat vals;
  inherit (config.networking) hostName;
in
{

  imports = [
    ../../modules/site-net
  ];

  # Merge in the site secrets
  site = config.repo.secrets.site.site;

  networking = {
    hostId = pkgs.lib.concatStringsSep "" (
      pkgs.lib.take 8 (pkgs.lib.stringToCharacters (builtins.hashString "sha256" hostName))
    );
    domain = config.site.data.networkDomain;
    useDHCP = false;
  };

  #sops.secrets."wg1/privateKey" = {
  #  owner = "root";
  #  group = "systemd-network";
  #  mode = "0640";
  #};
  systemd.network = {
    enable = true;
    wait-online = {
      anyInterface = false;
      ignoredInterfaces = [
        "ve-+"
        "wan0"
        "lan1"
        "ctr0"
        "wg0"
        "ve-mullvad"
      ];
    };
    config.routeTables.vpn = 200;

    links = {
      # rename all interface names to be easier to identify
      "10-wan0" = {
        # 00:1f.6 Ethernet controller: Intel Corporation Ethernet Connection (7) I219-V (rev 10)
        #matchConfig.Path = "pci-0000:00:1f.6";
        matchConfig.MACAddress = config.repo.secrets.local.wan0.mac;
        linkConfig.Name = "wan0";
      };
      "10-lan0" = {
        # 01:00.0 Ethernet controller: Mellanox Technologies MT27520 Family [ConnectX-3 Pro]
        # right SFP+ port
        matchConfig.MACAddress = config.repo.secrets.local.lan0.mac;
        linkConfig.Name = "lan0";
      };
      "10-lan1" = {
        # 01:00.0 Ethernet controller: Mellanox Technologies MT27520 Family [ConnectX-3 Pro]
        # left SFP+ port
        matchConfig.MACAddress = config.repo.secrets.local.lan1.mac;
        linkConfig.Name = "lan1";
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
            "wlp3s0"
            "gre0"
            "gretap0"
            "erspan0"
          ]
      ))
      // {
        # Enabled interfaces
        "30-wan0" = {
          matchConfig.Name = "wan0";
          address = [ config.repo.secrets.local.wan0.cidr ];
          networkConfig = {
            Gateway = config.repo.secrets.local.wan0.gateway;
            DHCPServer = false;
            # Try hard not to use ipv6 in case our ISP turns it on one day (hahaha, fat chance)
            # We don't want to suddenly start using it without planning
            IPv6AcceptRA = false;
            IPv6SendRA = false;
            IPv6Forwarding = false;
          };
          linkConfig = {
            MTUBytes = "1500";
            RequiredForOnline = "routable";
          };
        };

        "30-ve-mullvad" = {
          matchConfig = {
            Name = "ve-mullvad";
            Driver = "veth";
          };
          address = [ "10.4.0.1/24" ];
          networkConfig.DHCPServer = false;
          routes =
            [
              {
                Gateway = "10.4.0.2";
                GatewayOnLink = true; # prevents link from staying in "configuring" state
                Table = "vpn";
              }
              {
                Destination = "0.0.0.0/0";
                Type = "unreachable";
                Table = "vpn";
              }
            ]
            ++ (
              let
                deny = cidr: {
                  Destination = cidr;
                  Table = "vpn";
                  Type = "unreachable";
                };
                hostNets = (keys config.site.hosts.${hostName}.interfaces);
                cidr4s = (map (net: config.site.net.${net}.subnet4) hostNets);
                cidr6s = mapcat (net: (vals config.site.net.${net}.subnets6)) hostNets;
              in
              (map deny cidr4s) ++ (map deny cidr6s)
            );
        };
      };
  };
}
