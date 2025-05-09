{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  inherit (config.networking) hostName;
  inherit (lib.mori)
    containsKey
    some
    vals
    keys
    filter
    mapcat
    merge
    nth
    first
    ;
  nets = config.site.net;
  inSite = config.site.hosts ? ${hostName};
  hostConfig = config.site.hosts.${hostName};
  hostPhysicals = lib.naturalSort (
    keys (filter (_: iface: iface.type == "phys") hostConfig.interfaces)
  );
  hostBridges = lib.naturalSort (
    keys (filter (_: iface: iface.type == "bridge") hostConfig.interfaces)
  );
  hostGres = keys (filter (_: iface: iface.type == "gre") hostConfig.interfaces);
  hasStatic4 = net: (nets.${net}.hosts4 ? ${hostName});
  hasStatic6 = net: (some (v: containsKey v hostName) (vals nets.${net}.hosts6));
  defaultBridgeParent = first hostPhysicals;

  genNetDev = net: {
    "20-${net}" = {
      netdevConfig = {
        Kind = "macvlan";
        Name = "${net}";
      };
      macvlanConfig = {
        Mode = "bridge";
      };
    };
    "20-vlan-${net}" = {
      netdevConfig = {
        Kind = "vlan";
        Name = "vlan-${net}";
        MTUBytes = nets.${net}.mtu;
      };
      vlanConfig.Id = nets.${net}.vlan;
    };
  };

  genAddresses4 =
    net:
    let
      prefixLen = toString nets.${net}.subnet4Len;
    in
    lib.optionals (hasStatic4 net) (
      map (address: {
        Address = "${address}/${prefixLen}";
      }) nets.${net}.hosts4.${hostName}
    );

  genAddresses6 =
    net:
    lib.optionals (hasStatic6 net) (
      mapcat (hosts6: (map (addr: { Address = "${addr}/64"; }) hosts6.${hostName})) (
        vals nets.${net}.hosts6
      )
    );

  genAddresses = net: (genAddresses4 net) ++ (genAddresses6 net);

  genNet = net: {
    "30-${net}" = {
      matchConfig.Name = net;
      linkConfig =
        {
          RequiredForOnline = "routable";
        }
        // lib.optionalAttrs (hostConfig.interfaces.${net}.hwaddr != null) {
          MACAddress = hostConfig.interfaces.${net}.hwaddr;
        };
      networkConfig =
        {
          LLDP = true;
          EmitLLDP = true;
          DHCP = if nets.${net}.dhcp.enable && !(hasStatic4 net) then "ipv4" else false;
          DHCPServer = false;
          MulticastDNS = true;
          IPv6AcceptRA = !(hasStatic6 net) && !hostConfig.isRouter;
          IPv6SendRA = hostConfig.isRouter;
          IPv4Forwarding = true;
          IPv6Forwarding = true;
          #LinkLocalAddressing = "ipv6";
          LinkLocalAddressing = "no";
        }
        // lib.optionalAttrs (hostConfig.interfaces.${net}.gw4) {
          Gateway = nets.${net}.hosts4.${nets.${net}.dhcp.router};
        };
      addresses = genAddresses net;
      routes = hostConfig.interfaces.${net}.routes;
      routingPolicyRules = hostConfig.interfaces.${net}.routingPolicyRules;
      ipv6Prefixes = lib.optionals (hostConfig.isRouter) (
        map (prefix: {
          Prefix = prefix;
        }) (vals nets.${net}.subnets6)
      );
      ipv6SendRAConfig = lib.optionalAttrs (!(hasStatic6 net) && !hostConfig.isRouter) {
        # no dhcpv6 server is in use
        Managed = false;
        OtherInformation = false;
        RouterLifetimeSec = 1800;
        EmitDNS = true;
        DNS = "_link_local";
        DNSLifetimeSec = 3600;

      };
    };
    "20-vlan-${net}" = {
      matchConfig.Name = "vlan-${net}";
      networkConfig.LinkLocalAddressing = "no";
      networkConfig.MACVLAN = net;
      linkConfig.RequiredForOnline = "carrier";
    };
    #"20-ports-${net}" = {
    #  matchConfig.Name = "${net}-*";
    #  networkConfig.Bridge = net;
    #};
  };
  genGreDevs = net: {
    "30-${net}" = {
      netdevConfig = {
        Kind = "gre";
        Name = net;
        MTUBytes = "1480";
      };
      tunnelConfig = {
        Local = hostConfig.interfaces.${net}.gre.local;
        Remote = hostConfig.interfaces.${net}.gre.remote;
        Independent = true;
      };
    };
  };
  genGres = net: {
    "30-${net}" = {
      matchConfig.Name = "${net}";
      addresses = genAddresses net;
      routes = [
        {
          Destination = "::/0";
          Gateway = lib.my.cidrToIp (nth nets.${net}.hosts6.main.wan6-gw 0);
          GatewayOnLink = true;
        }
      ];
    };
  };
  genPhys = net: {
    "10-${net}" =
      let
        vlansForThisIface = (
          filter (
            bridgeName:
            if hostConfig.interfaces.${bridgeName}.parent != null then
              hostConfig.interfaces.${bridgeName}.parent == net
            else
              net == defaultBridgeParent
          ) hostBridges
        );
      in
      {
        matchConfig.Name = net;
        addresses = lib.optionals (containsKey nets.${net}.hosts4 hostName) (
          map (addr: {
            Address = "${addr}/${toString nets.${net}.subnet4Len}";
          }) nets.${net}.hosts4.${hostName}
        );
        networkConfig = {
          DHCPServer = false;
          VLAN = map (net: "vlan-${net}") vlansForThisIface;
          LinkLocalAddressing = false;
          LLDP = true;
          EmitLLDP = true;
        };
        linkConfig = {
          MTUBytes = nets.${net}.mtu;
          RequiredForOnline = "carrier";
        };
      };
  };
  reduce = fn: list: builtins.foldl' (result: item: result // (fn item)) { } list;
in
{

  config = lib.mkIf (inSite && hostConfig.role == "server") {
    networking.hostId = lib.my.generateHostId hostName;
    networking.useDHCP = false;
    services.timesyncd.enable = true;
    services.resolved.enable = lib.mkDefault true;
    boot.kernelModules = lib.mkIf (hostGres != [ ]) [ "ip_gre" ];
    systemd.network.config.networkConfig = {
      IPv4Forwarding = lib.mkDefault hostConfig.isRouter;
      IPv6Forwarding = lib.mkDefault hostConfig.isRouter;
    };

    systemd.network.enable = true;
    systemd.network.wait-online = {
      anyInterface = false;
      ignoredInterfaces = [
        "ve-+"
        "tailscale0"
        "wan0"
        "lan1"
        "ctr0"
        "wg0"
        "ve-mullvad"
        "ve-maddy"
      ];
    };

    systemd.network.netdevs = merge [
      (reduce genNetDev hostBridges)
      (reduce genGreDevs hostGres)
    ];
    systemd.network.networks = merge [
      (reduce genPhys hostPhysicals)
      (reduce genNet hostBridges)
      (reduce genGres hostGres)

      {
        "20-tailscale-ignore" = {
          matchConfig.Name = "tailscale*";
          linkConfig = {
            Unmanaged = "yes";
            RequiredForOnline = false;
          };
        };
      }
    ];
  };
}
