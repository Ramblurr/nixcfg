{ config, lib, ... }:
let
  inherit (lib.mori)
    first
    keys
    filter
    merge
    mapcat
    ;
  inherit (config.networking) hostName;

  fwLib = import ./helpers.nix { inherit lib; };
  inherit (fwLib)
    mkRules
    ;

  # ──────────────────────────────────────────────────────────────────
  # Computed values for the firewall
  # ──────────────────────────────────────────────────────────────────
  # The local zone is the the router/firewall machine itself
  # So this applies to traffic on the input chain, rather than the forward chain
  # Use this zone when allowing traffic to services on the router itself
  local_zone = config.networking.nftables.firewall.localZoneName;
  wan_interfaces = config.networking.nftables.firewall.zones.wan.interfaces;
  # lets me refer to zones in a build-time safe way
  zones = lib.flip lib.concatMapAttrs config.networking.nftables.firewall.zones (
    name: _: {
      "${name}" = name;
    }
  );

  iface_zone_defs = merge (
    map (name: { ${name}.interfaces = [ name ]; }) (keys config.site.hosts.${hostName}.interfaces)
  );
  internal_zones = keys config.site.hosts.${hostName}.interfaces;
  internal_interfaces = [
    "lan0"
  ] ++ keys (filter (_: iface: iface.type == "bridge") config.site.hosts.${hostName}.interfaces);

  lan0_ip = first config.site.net.lan0.hosts4.${hostName};

  # ──────────────────────────────────────────────────────────────────
  # Begin my actual config data
  # ──────────────────────────────────────────────────────────────────
  zone_defs = {
    wan.interfaces = [ config.repo.secrets.local.wan0.iface ];
    mullvad.interfaces = [ "ve-mullvad" ];
    maddy.interfaces = [ "ve-maddy" ];
  } // iface_zone_defs;
  port_forwards = {
    ntp = {
      comment = "force NTP for all interfaces";
      priority = 100;
      interfaces = internal_interfaces;
      protocols = [ "udp" ];
      destination = {
        address = "!= ${lan0_ip}";
        port = 123;
      };
      translation = {
        address = lan0_ip;
        port = 123;
      };
    };
    dns = {
      comment = "force DNS for iot and not";
      priority = 101;
      interfaces = [
        "iot"
        "inot"
      ];
      protocols = [
        "tcp"
        "udp"
      ];
      destination = {
        address = "!= ${lan0_ip}";
        port = 53;
      };
      translation = {
        address = lan0_ip;
        port = 53;
      };
    };

    plex = {
      priority = 102;
      interfaces = wan_interfaces;
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
      interfaces = wan_interfaces;
      protocols = [ "tcp" ];
      destination.port = 33399;
      translation = {
        address = "10.9.8.14";
        port = 33399;
      };
      comment = "Roon ARC";
    };

    zreplMali1 = {
      priority = 104;
      interfaces = wan_interfaces;
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
      interfaces = wan_interfaces;
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
      interfaces = internal_interfaces;
      comment = "maddy smtp relay";
      protocols = [ "tcp" ];
      destination.port = 25;
      translation = {
        address = config.repo.secrets.global.email.siteRelay;
        port = 25;
      };
    };
  };
  rules = {
    wan_to_lan_ingress = {
      from = [ zones.wan ];
      to = "all";
      ruleType = "policy";
      extraLines = mkRules [
        {
          comment = "allow roon arc";
          destPort = "roon_arc_ports";
          destAddr = "home_ops_ingress";
          proto = [ "tcp" ];
          extra = [ "counter" ];
        }
        {
          comment = "allow mali replication";
          destPort = "mali_replication_ports";
          destAddr = "mali_all";
        }
        {
          comment = "allow plex";
          destPort = "plex_server_ports";
          destAddr = "home_ops_ingress";
        }
        {
          destPort = "wireguard_ports";
          comment = "allow wireguard";
          extra = [ "counter" ];
        }
        # ''counter drop''
        ''counter log prefix "wan_ingress " drop''
      ];
    };

    wan_ingress = {
      from = "all";
      to = [
        zones.wan
        local_zone
      ];
      extraLines = mkRules [
        {
          comment = "allow plex";
          destPort = "plex_server_ports";
        }
        {
          destPort = "wireguard_ports";
          comment = "allow wireguard2";
          extra = [ "counter" ];
        }
      ];
    };

    wan_egress = {
      from = (lib.remove "vlan_vpn" internal_zones) ++ [
        zones.mullvad
        zones.maddy
      ];
      to = [ zones.wan ];
      verdict = "accept";
      late = true;
      masquerade = true;
    };

    wan_ipv6_egress = {
      from = [ zones.prim ];
      to = [
        #zones.wan6tun
      ];
      verdict = "accept";
    };

    lan_to_maddy = {
      from = [
        zones.mgmt
        zones.svc
        zones.prim
      ];
      to = [ zones.maddy ];
      allowedTCPPortRanges = [
        {
          from = 25;
          to = 25;
        }
      ];
    };

    lan_to_mullvad = {
      from = [ zones.prim ];
      to = [ zones.mullvad ];
      allowedTCPPortRanges = [
        {
          from = 1080;
          to = 1090;
        }
      ];
    };

    vpn_to_mullvad = {
      from = [ zones.vpn ];
      to = [ zones.mullvad ];
      extraLines = [
        ''counter accept''
      ];
    };

    vpn_backup = {
      # Nodes on my vpn vlan are allowed to talk to my borgmatic backup server
      from = [ zones.vpn ];
      to = [ zones.prim ];

      extraLines = mkRules [
        {
          comment = "allow vpn clients ssh to backup server";
          destPort = "ssh_ports";
          destAddr = "mali_prim";
        }
      ];
    };
    vpn_block = {
      # otherwise we block the vpn vlan from talking to the rest of the network
      from = [ zones.vpn ];
      to = internal_zones ++ [ local_zone ];
      extraLines = mkRules [
        {
          comment = "allow vpn clients to ntp server";
          destPort = "ntp_ports";
          destAddr = "router_untagged";
          proto = [ "udp" ];
        }
        ''counter log prefix "block_vpn_to_lan " reject with icmpx type no-route comment "Reject VPN traffic not going to Mullvad"''
      ];
    };

    dns = {
      # Allow all hosts to access the router's DNS server
      # except for the vpn vlan hosts, which should use the VPN's resolvers
      from = lib.remove "vlan_vpn" internal_zones;
      to = [ local_zone ];
      extraLines = mkRules [
        {
          comment = "allow dns";
          destPort = "dns_ports";
        }
      ];
    };

    dhcp = {
      from = "all";
      to = [
        local_zone
        zones.lan0
      ];
      extraLines = [
        ''udp sport 68 udp dport 67 counter accept comment "allow router to be dhcp server"''
      ];
    };

    ntp_and_mdns = {
      after = [
        "ct"
        "ssh"
      ];
      from = "all";
      to = [ local_zone ];
      allowedUDPPorts = [
        123
        5353
      ];
    };

    trusted_to_iot = {
      # I allow my trusted zones to initiate connections to my iot/not zones
      from = [
        zones.prim
        zones.mgmt
      ];
      to = [
        zones.iot
        zones.inot
      ];
      verdict = "accept";
    };

    iot_to_trusted = {
      # My iot/not vlans in general are not allowed to talk to my trusted zones
      # but there are a bunch of exceptions
      from = [
        zones.iot
        zones.inot
      ];
      to = [
        zones.mgmt
        zones.prim
      ];
      extraLines = mkRules [
        {
          comment = "allow bluesound";
          destAddr = "bluesound";
          destPort = "bluesound_ports";
          proto = [
            "tcp"
            "udp"
          ];
        }
        {
          comment = "allow roon";
          destAddr = "roon";
          destPort = "roon_ports";
          proto = [
            "tcp"
            "udp"
          ];
        }
        {
          comment = "allow octoprint to homeassistant";
          srcAddr = "octoprint";
          destAddr = "homeassistant";
          proto = [
            "tcp"
            "udp"
          ];
        }
        {
          comment = "allow wall dashboard to ingress";
          srcAddr = "wall_dashboards";
          destAddr = "home_ops_ingress";
          destPort = "http_ports";
          proto = [
            "tcp"
            "udp"
          ];
        }
        {
          comment = "allow wall dashboard to homeassistant";
          srcAddr = "wall_dashboards";
          destAddr = "homeassistant";
          destPort = "homeassistant_ports";
          proto = [
            "tcp"
            "udp"
          ];
        }
        {
          comment = "allow shellys to homeasssistant";
          destAddr = "homeassistant";
          destPort = "homeassistant_shelly_ports";
          srcAddr = "shellys";
        }
        {
          comment = "allow ha voices to homeasssistant";
          destAddr = "homeassistant";
          destPort = "homeassistant_ports";
          srcAddr = "havoice";
        }

        # block everything else
        ''counter log prefix "iot_trusted_block " reject with icmpx type admin-prohibited comment "Drop all other iot/not traffic to trusted zones"''
      ];
    };

    not_wan_exceptions = {
      # Devices in my not vlan are not allowed to talk to the internet
      # (but there are a few exceptions)
      from = [ zones.inot ];
      to = [ zones.wan ];

      extraLines = mkRules [
        {
          comment = "allow shellys to access firmware updates";
          srcAddr = "shellys";
        }
        ''counter log prefix "not_wan_block " reject with icmpx type admin-prohibited comment "NOT devices not allowed to access wan"''
      ];

    };

    syncthing = {
      from = [
        zones.prim
        zones.data
        zones.mgmt
      ];
      to = internal_zones;
      extraLines = mkRules [
        {
          comment = "allow syncthing";
          destPort = "syncthing_ports";
        }
      ];
    };

    roon_server = {
      from = [
        zones.prim
        zones.data
        zones.mgmt
      ];
      to = internal_zones ++ [
        zones.wan
        local_zone
      ];
      extraLines = mkRules [
        {
          comment = "allow roon";
          destPort = "roon_ports";
          srcAddr = "roon_server";
        }
      ];
    };

    get_out_of_my_firewall_logs = {
      # lots of software and devices like to do noisy discovery broadcasts
      # this is generally fine, they can do that, but I don't want to allow that traffic
      # on my router, and I certainly don't want to see it in the input chain logs of my firewall
      from = internal_zones;
      to = [
        local_zone
        zones.wan
      ];
      extraLines = mkRules [
        {
          destPort = "blocked_discovery_ports";
          proto = [ "udp" ];
          verdict = "reject";
        }
        {
          destPort = "blocked_discovery_tcp_ports";
          proto = [ "tcp" ];
          verdict = "reject";
        }
      ];
    };

    tailscale = {
      from = internal_zones;
      to = internal_zones ++ [ local_zone ];
      extraLines = mkRules [
        {
          destPort = "tailscale_dest_ports";
          proto = [ "udp" ];
        }
        {
          srcPort = "tailscale_src_ports";
          proto = [ "udp" ];
        }
      ];
    };

    #prim_to_mgmt = {
    #  from = [
    #    zones.prim
    #  ];
    #  to = [
    #    zones.mgmt
    #  ];
    #  extraLines = (
    #    map setRule [
    #      {
    #        comment = "allow home assistant backup";
    #        destAddr = "";
    #        srcAddr = "homeassistant";
    #      }
    #    ]
    #  );
    #};

    temporary_allow_inter_vlan = {
      from = internal_zones;
      to = internal_zones;
      late = true;
      verdict = "accept";
    };

    admin_devices = {
      # I allow my admin devices to go everywhere
      from = internal_zones;
      to = internal_zones ++ [ local_zone ];
      extraLines = mkRules [
        {
          destPort = "ssh_ports";
          srcAddr = "admin";
          comment = "allow admin access";
        }
        {
          destPort = "http_ports";
          srcAddr = "admin";
          comment = "allow admin web access";
        }
        {
          destPort = "admin_ports";
          srcAddr = "admin";
          comment = "allow admin access";
        }
      ];
    };
  };
in
{
  inherit
    local_zone
    zones
    internal_zones
    internal_interfaces
    port_forwards
    zone_defs
    rules
    ;
}
