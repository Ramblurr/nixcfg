{
  config,
  lib,
  pkgs,
  ...
}: let
  useMullvad = config.deviceSpecific.vpn.mullvad.enable;
  useTailscale = config.deviceSpecific.vpn.tailscale.enable;
in {
  config = lib.mkMerge [
    {
      networking = {
        firewall.enable = true;
        nftables.enable = true;
        firewall.logRefusedConnections = false;
      };
    }
    {
      # FIXME: allow mullvad custom dns
      networking.nftables.ruleset = ''
        ${lib.optionalString (useMullvad && useTailscale) ''
          table inet mullvad-tailscale-exclude {
          chain exclude-outgoing {
            type route hook output priority 0; policy accept;
          #  this breaks mullvad dns
          #  ip daddr 100.64.0.0/10 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 10.0.0.0/16 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 10.11.0.0/16 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip6 daddr fd7a:115c:a1e0::/48 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
          }
          chain allow-incoming {
            type filter hook input priority -100; policy accept;
            iifname "tailscale0" ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
          }
          chain exclude-dns {
            type filter hook output priority -10; policy accept;
            ip daddr 100.100.100.100 udp dport 53 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 100.100.100.100 tcp dport 53 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 10.0.0.1 udp dport 53 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 10.0.0.1 tcp dport 53 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
          }
          }
        ''}

          ${lib.optionalString useMullvad ''
          table inet mullvad-ssh-exclude {
            chain allowIncoming {
              type filter hook input priority -100; policy accept;
              tcp dport 22 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            }

            chain allowOutgoing {
              type route hook output priority -100; policy accept;
              tcp sport 22 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            }
          }
        ''}
      '';
    }
  ];
}
