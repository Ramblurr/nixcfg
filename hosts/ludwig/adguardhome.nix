{
  config,
  lib,
  pkgs,
  ...
}:
let
  site = import ./site.nix;
  inherit (config.services.adguardhome.settings) dns;
in
{
  services.adguardhome = {
    enable = true;
    mutableSettings = false;
    host = site.lan.dns;
    port = 80;
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
  sops.secrets.admin-password = { };
  systemd.services.adguardhome = {
    serviceConfig.LoadCredential = [
      "admin-password:${config.sops.secrets.admin-password.path}"
    ];
    preStart = lib.mkAfter ''
      hash=$(${lib.getExe pkgs.mkpasswd} -m bcrypt -R 10 -s < "$CREDENTIALS_DIRECTORY/admin-password")
      printf 'users:\n  - name: seed\n    password: %s\n' "$hash" >> "$STATE_DIRECTORY/AdGuardHome.yaml"
    '';
  };
  networking.firewall.interfaces.${site.lan.interface} = {
    allowedTCPPorts = [
      dns.port
      config.services.adguardhome.port
    ];
    allowedUDPPorts = [ dns.port ];
  };
}
