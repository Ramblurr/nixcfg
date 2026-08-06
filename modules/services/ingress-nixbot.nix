{
  config,
  lib,
  ...
}:

let
  cfg = config.modules.services.ingress-nixbot;
  homeDomain = config.repo.secrets.global.domain.home;
  workDomain = config.repo.secrets.global.domain.work;
in
{
  options.modules.services.ingress-nixbot.enable = lib.mkEnableOption "Nixbot ingress";

  config = lib.mkIf cfg.enable {
    # Nixbot runs on debord. Dewey terminates TLS for internal clients and for
    # the james gost tunnel, then proxies over the prim VLAN to debord.
    # See hosts/debord/nixbot.nix.
    modules.services.ingress.virtualHosts."ci.${workDomain}" = {
      acmeHost = workDomain;
      upstream = "http://debord.prim.${homeDomain}:${toString config.repo.secrets.home-ops.ports.nixbot}";
      extraConfig = ''
        location = /robots.txt {
          default_type text/plain;
          add_header X-Robots-Tag "noindex, nofollow, noarchive" always;
          return 200 "User-agent: *\nDisallow: /\n";
        }
      '';
      upstreamExtraConfig = ''
        # GitHub webhook payloads can be up to 25 MB.
        client_max_body_size 25m;
        proxy_connect_timeout 120s;
        proxy_send_timeout 120s;
        # Long timeout keeps SSE log streams alive; buffering would stall SSE.
        proxy_read_timeout 3600s;
        proxy_buffering off;
        add_header X-Robots-Tag "noindex, nofollow, noarchive" always;
      '';
    };
  };
}
