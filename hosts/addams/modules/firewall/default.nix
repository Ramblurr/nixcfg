{ config, lib, ... }:
let

  # Note on naming:
  # nftables doesn't support naming sets with dashes, so I use underscores
  # to be consistent I use underscores in all the names, even though elsewhere in my nix config
  # I use dashes or camel case.

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

  fwLib = import ./helpers.nix { inherit lib; };
  #addrs = import ./address_groups.nix;

  # The names of all my vlans prefixed with vlan_ example: vlan_guest, vlan_iot
  vlan_zones = lib.mapAttrsToList (name: v: "vlan_${name}") config.repo.secrets.local.vlan;

  internal_zones = vlan_zones ++ [ "untagged" ];
  # the iface names of all my vlans, example: vliot50
  vlan_interfaces = lib.mapAttrsToList (name: v: "me-${name}") config.repo.secrets.local.vlan;
  internal_interfaces = vlan_interfaces ++ [ config.repo.secrets.local.untagged.iface ];

  port_forwards = {
    #ntp = {
    #  priority = 100;
    #  interfaces = internal_interfaces;
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
    #maddy-smtp-relay = {
    #  priority = 106;
    #  interfaces = internal_interfaces;
    #  comment = "maddy smtp relay";
    #  protocols = [ "tcp" ];
    #  destination.port = 25;
    #  translation = {
    #    address = "10.4.0.2";
    #    port = 25;
    #  };
    #};
  };
  prefixStringLines =
    prefix: str: lib.concatMapStringsSep "\n" (line: prefix + line) (lib.splitString "\n" str);

  indent = prefixStringLines "  ";
  dropRule = label: {
    after = lib.mkForce [ "veryLate" ];
    before = lib.mkForce [ "end" ];
    rules = lib.singleton ''counter log prefix "default_drop_${label} " reject comment "Default drop rule for ${label} chain"'';
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
    ruleset = lib.mkBefore ''
      table inet firewall {
      ${indent (builtins.readFile ./config/sets_ports.nft)}
      ${indent (builtins.readFile ./config/sets_addresses.nft)}
      }
    '';

    chains = {
      prerouting.nat = {
        after = [ "hook" ];
        rules = fwLib.mkPortForwards port_forwards;
      };
      input.drop = dropRule "input";
      forward.drop = dropRule "forward";
    };
    firewall = {
      enable = true;
      snippets = {
        nnf-common.enable = false;
        nnf-conntrack.enable = true;
        nnf-default-stopRuleset.enable = true;
        nnf-drop.enable = false;
        nnf-loopback.enable = true;
        nnf-dhcpv6.enable = false;
        nnf-icmp.enable = true;
        nnf-ssh.enable = true;
        nnf-nixos-firewall.enable = false;
      };
      zones =
        {
          wan.interfaces = [ config.repo.secrets.local.wan0.iface ];
          untagged.interfaces = [ config.repo.secrets.local.untagged.iface ];
          mullvad.interfaces = [ "ve-mullvad" ];
        }
        #// lib.flip lib.concatMapAttrs addrs (
        #  name: l: {
        #    "${name}".ipv4Addresses = l;
        #  }
        #)
        // lib.flip lib.concatMapAttrs config.repo.secrets.local.vlan (
          vlanName: v: {
            "vlan_${vlanName}".interfaces = [ "me-${vlanName}" ];
          }
        );

      rules = {
        wan_ingress = {
          from = [ zones.wan ];
          to = "all";
          ruleType = "policy";
          extraLines = [
            (fwLib.setRule {
              comment = "allow roon arc";
              destPort = "roon_arc_ports";
              destAddr = "home_ops_ingress";
            })

            (fwLib.setRule {
              comment = "allow mali replication";
              destPort = "mali_replication_ports";
              destAddr = "mali_all";
            })

            (fwLib.setRule {
              comment = "allow plex";
              destPort = "plex_server_ports";
              destAddr = "home_ops_ingress";
            })
            ''counter drop''
          ];
        };

        lan_to_mullvad = {
          from = [ zones.vlan_prim ];
          to = [ zones.mullvad ];
          allowedTCPPortRanges = [
            {
              from = 1080;
              to = 1090;
            }
          ];
        };

        vpn_to_mullvad = {
          from = [ zones.vlan_vpn ];
          to = [ zones.mullvad ];
          extraLines = [
            ''counter accept''
          ];
        };

        vpn_to_lan_other = {
          from = [ zones.vlan_vpn ];
          to = internal_zones ++ [ local_zone ];
          extraLines = [
            ''counter log prefix "block_vpn_to_lan " reject with icmpx type no-route comment "Reject VPN traffic not going to Mullvad"''
          ];
        };

        vpn_backup = {
          # Nodes on my vpn vlan are allowed to talk to my borgmatic backup server
          from = [ zones.vlan_vpn ];
          to = [ zones.vlan_prim ];

          extraLines = (
            map fwLib.setRule [
              {
                comment = "allow vpn clients ssh to backup server";
                destPort = "ssh_ports";
                destAddr = "mali_prim";
              }
            ]
          );
        };

        dns = {
          from = lib.remove "vlan_vpn" internal_zones;
          to = [ local_zone ];
          extraLines = (
            map fwLib.setRule [
              {
                comment = "allow dns";
                destPort = "dns_ports";
              }
            ]
          );
        };

        dhcp = {
          from = "all";
          to = [
            local_zone
            zones.untagged
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
          from = [
            zones.vlan_prim
            zones.vlan_mgmt
          ];
          to = [
            zones.vlan_iot
            zones.vlan_not
          ];
          verdict = "accept";
        };

        syncthing = {
          from = [
            zones.vlan_prim
            zones.vlan_data
            zones.vlan_mgmt
          ];
          to = internal_zones;
          extraLines = (
            map fwLib.setRule [
              {
                comment = "allow syncthing";
                destPort = "syncthing_ports";
              }
            ]
          );
        };

        plex = {
          from = "all";
          to = [
            zones.wan
            local_zone
          ];
          extraLines = (
            map fwLib.setRule [
              {
                comment = "allow plex";
                destPort = "plex_server_ports";
              }
            ]
          );
        };
        roon_server = {
          from = [
            zones.vlan_prim
            zones.vlan_data
            zones.vlan_mgmt
          ];
          to = internal_zones ++ [
            zones.wan
            local_zone
          ];
          extraLines = (
            map fwLib.setRule [
              {
                comment = "allow roon";
                destPort = "roon_ports";
                srcAddr = "roon_server";
              }
            ]
          );
        };

        block_discovery = {
          # lots of software and devices like to broadcast/do discovery
          # which is fine inside their vlan,
          # but i dont want to see that in the input chain of my router firewall
          from = [
            zones.vlan_prim
            zones.vlan_mgmt
            zones.vlan_data
            zones.vlan_iot
            zones.vlan_not
            zones.untagged
          ];
          to = [
            local_zone
            zones.wan
          ];
          extraLines = [
            (fwLib.setRule {
              destPort = "blocked_discovery_ports";
              proto = [ "udp" ];
              verdict = "reject";
            })
            (fwLib.setRule {
              destPort = "blocked_discovery_tcp_ports";
              proto = [ "tcp" ];
              verdict = "reject";
            })
          ];
        };

        tailscale = {
          from = internal_zones;
          to = internal_zones ++ [ local_zone ];
          extraLines = [
            (fwLib.setRule {
              destPort = "tailscale_dest_ports";
              proto = [ "udp" ];
            })
            (fwLib.setRule {
              srcPort = "tailscale_src_ports";
              proto = [ "udp" ];
            })
          ];
        };

        #prim_to_mgmt = {
        #  from = [
        #    zones.vlan_prim
        #  ];
        #  to = [
        #    zones.vlan_mgmt
        #  ];
        #  extraLines = (
        #    map fwLib.setRule [
        #      {
        #        comment = "allow home assistant backup";
        #        destAddr = "";
        #        srcAddr = "homeassistant";
        #      }
        #    ]
        #  );
        #};
        iot_to_trusted = {
          from = [
            zones.vlan_iot
            zones.vlan_not
          ];
          to = [
            zones.vlan_mgmt
            zones.vlan_prim
          ];
          extraLines =
            (map fwLib.setRule [
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
            ])
            ++ [
              ''counter log prefix "iot_trusted_block " reject with icmpx type admin-prohibited comment "Drop all other iot/not traffic to trusted zones"''
            ];
        };

        not_wan_exceptions = {
          from = [ zones.vlan_not ];
          to = [ zones.wan ];

          extraLines =
            (map fwLib.setRule [
              {
                comment = "allow shellys to access firmware updates";
                srcAddr = "shellys";
              }
            ])
            ++ [
              ''counter log prefix "not_wan_block " reject with icmpx type admin-prohibited comment "NOT devices not allowed to access wan"''
            ];

        };

        temporary_allow_inter_vlan = {
          from = internal_zones;
          to = internal_zones;
          late = true;
          verdict = "accept";
        };

        workstation = {
          from = internal_zones;
          to = [ local_zone ];
          early = true;
          extraLines = map fwLib.setRule [
            {
              destPort = "ssh_ports";
              srcAddr = "admin";
              comment = "allow admin access";
            }
          ];
        };

        wan_egress = {
          from = internal_zones ++ [ zones.mullvad ];
          to = [ zones.wan ];
          verdict = "accept";
          late = true;
          masquerade = true;
        };
      };
    };
  };
}

# 154 good generation
