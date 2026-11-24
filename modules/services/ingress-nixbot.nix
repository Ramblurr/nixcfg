{
  config,
  lib,
  ...
}:

let
  cfg = config.modules.services.ingress-nixbot;
  workDomain = config.repo.secrets.global.domain.work;
in
{
  options.modules.services.ingress-nixbot.enable = lib.mkEnableOption "Nixbot ingress";

  config = lib.mkIf cfg.enable {
    # Nixbot runs on mootforge. Dewey Caddy terminates TLS and proxies over the
    # mgmt VLAN. James HAProxy selects this hostname for James public exposure.
    # See the mootforge Nixbot configuration.
    site.gatus.endpoints = [
      {
        name = "Nixbot CI";
        group = config.site.gatus.groups.work;
        url = "https://ci.${workDomain}/";
      }
    ];

    modules.services.caddy.routes.ci = {
      publicHost = "ci.${workDomain}";
      upstream = "http://${builtins.head config.site.net.mgmt.hosts4.mootforge}:${toString config.repo.secrets.home-ops.ports.nixbot}";
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
