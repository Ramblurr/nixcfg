{ config, lib, ... }:
let
  # The local zone is the the router/firewall machine itself
  # So this applies to traffic on the input chain, rather than the forward chain
  # Use this zone when allowing traffic to services on the router itself
  localZone = config.networking.nftables.firewall.localZoneName;

  helpers = import ./helpers.nix;

  wanInterfaces = config.networking.nftables.firewall.zones.wan.interfaces;
  allZones = [
    "local"
    "guest"
    "prim"
    "mgmt"
    "data"
    "iot"
    "not"
  ];

  internalInterfaces = [
    "lan0"
    "vlguest3"
    "vlprim4"
    "vlmgmt9"
    "vldata11"
    "vliot50"
    "vlnot60"
  ];

  portForwards = {
    #ntp = {
    #  priority = 100;
    #  interfaces = internalInterfaces;
    #  protocols = [ "udp" ];
    #  destination = {
    #    address = "!= 192.168.10.1";
    #    port = 123;
    #  };
    #  translation = {
    #    address = "192.168.10.1";
    #    port = 123;
    #  };
    #  comment = "force NTP for all interfaces";
    #};
    #dns = {
    #  enable = false;
    #  priority = 101;
    #  interfaces = [
    #    "vliot50"
    #    "vlnot60"
    #  ];
    #  protocols = [
    #    "tcp"
    #    "udp"
    #  ];
    #  comment = "force DNS for iot and not";
    #  destination = {
    #    address = "";
    #    port = 53;
    #  };
    #  translation = {
    #    address = "";
    #    port = 53;
    #  };
    #};
    plex = {
      priority = 102;
      interfaces = wanInterfaces;
      protocols = [
        "tcp"
        "udp"
      ];
      destination.port = 32400;
      translation.address = "10.9.8.14";
      translation.port = 32400;
      comment = "port forward for plex";
    };
    roonArc = {
      priority = 103;
      interfaces = wanInterfaces;
      protocols = [ "udp" ];
      destination.port = 33399;
      translation = {
        address = "10.9.8.14";
        port = 33399;
      };
      comment = "Roon ARC UDP";
    };

    zreplMali1 = {
      priority = 104;
      interfaces = wanInterfaces;
      protocols = [ "tcp" ];
      destination.port = 3478;
      translation = {
        address = "10.9.10.10";
        port = 3478;
      };
      comment = "mali zrepl replication";
    };

    zreplMali2 = {
      priority = 105;
      interfaces = wanInterfaces;
      protocols = [ "tcp" ];
      destination.port = 3479;
      translation = {
        address = "10.9.10.10";
        port = 3479;
      };
      comment = "mali zrepl replication2";
    };
    maddy-smtp-relay = {
      priority = 106;
      interfaces = internalInterfaces;
      comment = "maddy smtp relay";
      protocols = [ "tcp" ];
      destination.port = 25;
      translation = {
        address = "10.4.0.2";
        port = 25;
      };
    };
  };

in
{
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = true;
    "net.ipv6.conf.all.forwarding" = false;
  };
  networking.firewall.enable = false;
  networking.nat.enable = false;
  networking.nftables.enable = true;
  networking.nftables = {
    chains = {
      prerouting = {
        "nat" = {
          after = [ "hook" ];
          rules = helpers.mkPortForwards portForwards;
        };
      };
    };
    firewall = {
      enable = true;
      snippets.nnf-common.enable = true;
      zones.wan = {
        interfaces = [ "wan0" ];
      };
      zones.local = {
        ipv4Addresses = [ config.repo.secrets.local.vlan.local.cidr ];
      };
      zones.guest = {
        ipv4Addresses = [ config.repo.secrets.local.vlan.guest.cidr ];
      };
      zones.prim = {
        ipv4Addresses = [ config.repo.secrets.local.vlan.prim.cidr ];
      };
      zones.mgmt = {
        ipv4Addresses = [ config.repo.secrets.local.vlan.mgmt.cidr ];
      };
      zones.data = {
        ipv4Addresses = [ config.repo.secrets.local.vlan.data.cidr ];
      };
      zones.iot = {
        ipv4Addresses = [ "10.8.50.0/23" ];
      };
      zones.not = {
        ipv4Addresses = [ "10.8.60.0/23" ];
      };
      rules = {
        wanIngress = {
          from = [ "wan" ];
          to = "all";
          ruleType = "policy";
          extraLines = [
            ''counter drop''
          ];
        };
        wanEgress = {
          from = allZones;
          to = [ "wan" ];
          verdict = "accept";
          masquerade = true;
        };
        dns = {
          from = allZones;
          to = [ localZone ];
          allowedTCPPorts = [
            53
            853
          ];
          allowedUDPPorts = [
            53
            853
          ];
          verdict = "accept";
        };
        temporary = {
          from = allZones;
          to = allZones;
          verdict = "accept";
        };
      };
    };
  };
}
