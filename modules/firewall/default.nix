{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
let
  cfg = config.modules.firewall;
  username = config.modules.users.primaryUser.username;
  withImpermanence = config.modules.impermanence.enable;
  useMullvad = config.modules.vpn.mullvad.enable;
  useTailscale = config.modules.vpn.tailscale.enable;
in
{
  options.modules.firewall = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    networking = {
      firewall.enable = true;
      nftables.enable = true;
      firewall.logRefusedConnections = false;
    };
    # FIXME: allow mullvad custom dns
    networking.nftables.ruleset = ''
      ${optionalString (useMullvad && useTailscale) ''
        table inet mullvad-tailscale-exclude {
          chain exclude-outgoing {
            type route hook output priority 0; policy accept;
          #  this breaks mullvad dns
          #  ip daddr 100.64.0.0/10 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;

            # this should work, but also doesn't
            ip daddr 100.64.0.0/32 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 100.64.0.8/29 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 100.64.0.16/28 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 100.64.0.32/27 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 100.64.0.64/26 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 100.64.0.128/25 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 100.64.1.0/24 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 100.64.2.0/23 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 100.64.4.0/22 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 100.64.8.0/21 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 100.64.16.0/20 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 100.64.32.0/19 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 100.64.64.0/18 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 100.64.128.0/17 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 100.65.0.0/16 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 100.66.0.0/15 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 100.68.0.0/14 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 100.72.0.0/13 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 100.80.0.0/12 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            ip daddr 100.96.0.0/11 ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
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

        ${optionalString useMullvad ''
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
              192.168.1.3,
              10.9.4.4,
              10.10.10.53,
              10.10.12.53
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
              10.10.10.0/23,
              10.10.12.0/23,
              192.168.1.0/24,
              192.168.8.0/22
            }
            chain exclude-local-lan {
                type route hook output priority 0; policy accept;
                ip daddr $EXCLUDED_IPS ct mark set 0x00000f41 meta mark set 0x6d6f6c65;
            }
          }
        ''}
    '';
  };
}
