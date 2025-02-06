{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.repo.secrets.local;
in
{
  environment.persistence."/persist".directories = [ "/var/lib/pdns" ];
  services.dnsdist = {
    enable = true;
    extraConfig =
      let
        transformDomainToRegex =
          domain:
          let
            escapedDomain = lib.replaceStrings [ "." ] [ "\\\\." ] domain;
          in
          "(^|\\\\.)${escapedDomain}$";
      in
      ''
        -- disable security status polling via DNS
        setSecurityPollSuffix("")

        -- udp/tcp dns listening
        setLocal("0.0.0.0:53", {})

        -- Local Authoritiative Zones (powerdns)
        newServer({
          address = "127.0.0.1:8853",
          pool = "powerdns",
        })

        -- NextDNS over TLS
        newServer({
          address = "${cfg.dns.upstream1}",
          tls = "openssl",
          subjectName = "dns.nextdns.io",
          dohPath = "${cfg.dns.upstreamDOHPath}",
          validateCertificates = true,
          checkInterval = 10,
          checkTimeout = 2000,
          pool = "nextdns_trusted"
        })
        newServer({
          address = "${cfg.dns.upstream2}",
          tls = "openssl",
          subjectName = "dns.nextdns.io",
          dohPath = "${cfg.dns.upstreamDOHPath}",
          validateCertificates = true,
          checkInterval = 10,
          checkTimeout = 2000,
          pool = "nextdns_trusted"
        })

        -- Enable caching
        pc = newPacketCache(10000, {
          maxTTL = 86400,
          minTTL = 0,
          temporaryFailureTTL = 60,
          staleTTL = 60,
          dontAge = false
        })
        getPool(""):setCache(pc)


        -- Request logging, uncomment to log DNS requests/responses to stdout
        -- addAction(AllRule(), LogAction("", false, false, true, false, false))
        -- addResponseAction(AllRule(), LogResponseAction("", false, true, false, false))

        -- Routing rules
        addAction(RegexRule('${transformDomainToRegex config.repo.secrets.global.domain.home}'), PoolAction('powerdns'))
        addAction(RegexRule('${transformDomainToRegex config.repo.secrets.global.domain.work}'), PoolAction('powerdns'))
        ${builtins.concatStringsSep "\n  " (
          map (zone: "addAction('${zone}', PoolAction('powerdns'))") config.repo.secrets.local.reverseZones
        )}
        addAction(AllRule(), PoolAction("nextdns_trusted"))
      '';
  };
}
