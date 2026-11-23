{ config, lib, ... }:
let
  cfg = config.modules.microvm-host.isolatedTaps;
  nonPublic = import ../../config/non-public-ipv4.nix;
  nftSet = values: "{ ${lib.concatStringsSep ", " values} }";
  taps = nftSet (lib.mapAttrsToList (_: guest: ''"${guest.tapId}"'') cfg.guests);
  uplinks = nftSet (map (iface: ''"${iface}"'') cfg.uplinks);
  subnets = nftSet (
    lib.mapAttrsToList (_: guest: "${guest.hostAddress}/${toString guest.prefixLength}") cfg.guests
  );
in
{
  options.modules.microvm-host.isolatedTaps = {
    uplinks = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Host interfaces through which isolated guests reach the public internet.";
    };
    guests = lib.mkOption {
      default = { };
      description = ''
        Point-to-point tap links, keyed by guest name, for guests that may
        reach only the public internet. The host drops all their traffic to
        itself, to private, CGNAT, link-local and multicast ranges and all
        IPv6, and masquerades the rest out of the uplinks.
      '';
      type = lib.types.attrsOf (
        lib.types.submodule {
          options = {
            tapId = lib.mkOption {
              type = lib.types.strMatching "[a-zA-Z0-9-]{1,15}";
              description = "Tap interface name; the guest's microvm.interfaces id.";
            };
            hostAddress = lib.mkOption {
              type = lib.types.str;
              description = "Host IPv4 address on the tap, the guest's gateway.";
            };
            prefixLength = lib.mkOption {
              type = lib.types.ints.between 1 32;
              default = 30;
              description = "Prefix length of the tap subnet.";
            };
          };
        }
      );
    };
  };

  config = lib.mkIf (cfg.guests != { }) {
    assertions = [
      {
        assertion = cfg.uplinks != [ ];
        message = "modules.microvm-host.isolatedTaps.uplinks must name at least one uplink interface.";
      }
      {
        assertion = config.systemd.network.enable;
        message = "modules.microvm-host.isolatedTaps assigns tap addresses with systemd-networkd.";
      }
    ];

    boot.kernel.sysctl."net.ipv4.conf.all.forwarding" = true;

    systemd.network.networks = lib.mapAttrs' (
      _: guest:
      lib.nameValuePair "20-${guest.tapId}" {
        matchConfig.Name = guest.tapId;
        address = [ "${guest.hostAddress}/${toString guest.prefixLength}" ];
        networkConfig = {
          ConfigureWithoutCarrier = true;
          LinkLocalAddressing = "no";
          IPv6AcceptRA = false;
        };
        linkConfig.RequiredForOnline = "no";
      }
    ) cfg.guests;

    networking.nftables.enable = true;
    networking.nftables.tables.microvm-isolated-taps = {
      family = "inet";
      content = ''
        chain input {
          type filter hook input priority filter; policy accept;
          iifname ${taps} counter drop
        }
        chain forward {
          type filter hook forward priority filter; policy accept;
          iifname ${taps} meta nfproto ipv6 counter drop
          iifname ${taps} ip daddr ${nftSet nonPublic} counter drop
          iifname ${taps} oifname ${uplinks} accept
          iifname ${taps} counter drop
          oifname ${taps} ct state { established, related } accept
          oifname ${taps} counter drop
        }
        chain postrouting {
          type nat hook postrouting priority srcnat; policy accept;
          meta nfproto ipv4 ip saddr ${subnets} oifname ${uplinks} masquerade
        }
      '';
    };
  };
}
