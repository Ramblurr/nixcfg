{
  config,
  pkgs,
  lib,
  ...
}:
{

  networking = {
    hostId = pkgs.lib.concatStringsSep "" (
      pkgs.lib.take 8 (
        pkgs.lib.stringToCharacters (builtins.hashString "sha256" config.networking.hostName)
      )
    );
    domain = config.repo.secrets.global.domain.home;
    useDHCP = false;
  };
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
    netdevs = lib.flip lib.concatMapAttrs config.repo.secrets.local.vlan (
      vlanName: vlanCfg: {
        "30-vlan-${vlanName}" = {
          netdevConfig = {
            Kind = "vlan";
            Name = vlanCfg.iface;
            MTUBytes = if vlanCfg ? mtu then vlanCfg.mtu else "1500";
          };
          vlanConfig.Id = vlanCfg.id;
        };

        # Create a MACVTAP on the router, so that it can communicate with
        # its virtualized guests on the same interface.
        "40-me-${vlanName}" = {
          netdevConfig = {
            Name = "me-${vlanName}";
            Kind = "macvlan";
          };
          extraConfig = ''
            [MACVLAN]
            Mode=bridge
          '';
        };
      }
    );
    networks =
      {
        # Disabled interfaces
        "30-lan1" = {
          matchConfig.Name = "lan1";
          networkConfig.ConfigureWithoutCarrier = true;
          linkConfig.ActivationPolicy = "always-down";
          networkConfig.DHCPServer = false;
        };
        "30-wlp3s0" = {
          # This is the builtin wifi
          matchConfig.Name = "wlp3s0";
          linkConfig = {
            Unmanaged = "yes";
            ActivationPolicy = "down";
          };
        };

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

        "30-lan0" = {
          matchConfig.Name = "lan0";
          address = [
            config.repo.secrets.local.untagged.cidr
          ] ++ config.repo.secrets.local.untagged.extraAddrs;
          networkConfig.DHCPServer = false;
          linkConfig = {
            MTUBytes = "9000";
            RequiredForOnline = "carrier";
          };
          vlan = lib.mapAttrsToList (name: v: v.iface) config.repo.secrets.local.vlan;
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
                cidrs = lib.mapAttrsToList (name: v: v.cidr) config.repo.secrets.local.vlan;
              in
              map deny cidrs
            );
        };

      }
      // lib.flip lib.concatMapAttrs config.repo.secrets.local.vlan (
        vlanName: vlanCfg: {
          "30-vlan-${vlanName}" = {
            matchConfig.Name = vlanCfg.iface;
            # This interface should only be used from attached macvlans.
            # So don't acquire a link local address and only wait for
            # this interface to gain a carrier.
            networkConfig.LinkLocalAddressing = "no";
            networkConfig.MACVLAN = "me-${vlanName}";
            linkConfig.RequiredForOnline = "carrier";
          };
          "40-me-${vlanName}" = {
            address = lib.optionals (!(vlanCfg ? addresses)) (
              [ vlanCfg.cidr ] ++ (if vlanCfg ? extraAddrs then vlanCfg.extraAddrs else [ ])
            );
            addresses = lib.optionals (vlanCfg ? addresses) vlanCfg.addresses;
            matchConfig.Name = "me-${vlanName}";
            networkConfig = {
              IPv4Forwarding = "yes";
              MulticastDNS = true;
              DHCPServer = false;
            } // lib.optionalAttrs (vlanCfg ? networkConfig) vlanCfg.networkConfig;
            routes = lib.optionals (vlanCfg ? routes) vlanCfg.routes;
            routingPolicyRules = lib.optionals (vlanCfg ? routingPolicyRules) vlanCfg.routingPolicyRules;
            linkConfig.RequiredForOnline = "routable";
          };
        }
      );
  };
}
