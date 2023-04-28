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
            define TAILSCALE_RESOLVER_ADDRS = {
              # tailscale dns
              100.100.100.100
            }
            chain exclude-dns {
              type filter hook output priority -10; policy accept;
              ip daddr $TAILSCALE_RESOLVER_ADDRS udp dport 53 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
              ip daddr $TAILSCALE_RESOLVER_ADDRS tcp dport 53 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            }
          }
        ''}

          ${lib.optionalString useMullvad ''
          table inet mullvad-local-exclude {
            chain allow-incoming-ssh {
              type filter hook input priority -100; policy accept;
              tcp dport 22 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            }

            chain allow-outgoing-ssh {
              type route hook output priority -100; policy accept;
              tcp sport 22 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            }

            define LOCAL_RESOLVER_ADDRS = {
              # local dns
              192.168.1.3,
              # local dns
              10.9.4.4
            }
            chain exclude-local-dns {
              type filter hook output priority -10; policy accept;
              ip daddr $LOCAL_RESOLVER_ADDRS udp dport 53 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
              ip daddr $LOCAL_RESOLVER_ADDRS tcp dport 53 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            }
            define EXCLUDED_IPS = {
              10.8.3.1/24,
              10.9.4.1/22,
              10.9.8.1/23,
              10.9.10.1/23,
              10.8.50.1/23,
              10.8.60.1/23,
              10.5.0.0/24,
              10.10.10.0/23
            }
            chain exclude-local-lan {
                type route hook output priority 0; policy accept;
                ip daddr $EXCLUDED_IPS ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            }
          }
        ''}
      '';
    }
  ];
}
