{
  config,
  ...
}:
#
# Replaces the adguardhome jail at the same address. The web UI has no users,
# so it listens on loopback only; reach it with an SSH tunnel.
let
  site = import ./site.nix;
  inherit (config.services.adguardhome.settings) dns;
in
{
  services.adguardhome = {
    enable = true;
    mutableSettings = false;
    host = "127.0.0.1";
    settings = {
      dns = {
        bind_hosts = [ site.lan.dns ];
        port = 53;
        upstream_dns = [
          "https://dns.cloudflare.com/dns-query"
          "https://dns.google/dns-query"
          "https://dns.quad9.net/dns-query"
        ];
        bootstrap_dns = [
          "9.9.9.10"
          "149.112.112.10"
          "2620:fe::10"
        ];
      };
      filtering = {
        protection_enabled = true;
        filtering_enabled = true;
        safebrowsing_enabled = true;
        rewrites = config.repo.secrets.local.ludwig.dnsRewrites;
      };
      filters = [
        {
          enabled = true;
          id = 1;
          name = "AdGuard DNS filter";
          url = "https://adguardteam.github.io/AdGuardSDNSFilter/Filters/filter.txt";
        }
        {
          enabled = true;
          id = 2;
          name = "AdAway Default Blocklist";
          url = "https://adaway.org/hosts.txt";
        }
        {
          enabled = true;
          id = 1676978879;
          name = "Phishing URL Blocklist (PhishTank and OpenPhish)";
          url = "https://adguardteam.github.io/HostlistsRegistry/assets/filter_30.txt";
        }
      ];
    };
  };
  networking.firewall.interfaces.${site.lan.interface} = {
    allowedTCPPorts = [ dns.port ];
    allowedUDPPorts = [ dns.port ];
  };
}
