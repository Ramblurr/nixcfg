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
    modules.services.caddy.routes.ci = {
      publicHost = "ci.${workDomain}";
      upstream = "http://debord.prim.${homeDomain}:${toString config.repo.secrets.home-ops.ports.nixbot}";
      requestBodyMaxSize = "25MB";
      responseHeaders.X-Robots-Tag = "noindex, nofollow, noarchive";
      dialTimeout = "120s";
      flushInterval = "-1";
      staticResponses."/robots.txt" = {
        body = "User-agent: *\\nDisallow: /\\n";
        headers.X-Robots-Tag = "noindex, nofollow, noarchive";
      };
    };
  };
}
