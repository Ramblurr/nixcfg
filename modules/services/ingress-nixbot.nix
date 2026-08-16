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
    # Nixbot runs on Debord. Dewey Caddy terminates TLS and proxies over the
    # prim VLAN. James HAProxy selects this hostname for James public exposure.
    # See the Debord Nixbot configuration.
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
