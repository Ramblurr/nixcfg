{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.repo.secrets.global) domain email;
  caddyEdgeEnabled = config.modules.services.caddy-security.edge.enable;
  legacyAcmeEnabled = config.modules.services.ingress.legacyAcme.enable;
in
{
  security.acme = lib.mkIf legacyAcmeEnabled {
    acceptTerms = true;
    defaults = {
      email = email.acme;
      credentialFiles."DESEC_TOKEN_FILE" = config.sops.secrets.desec_api_token.path;
      dnsProvider = "desec";
      environmentFile = pkgs.writeText "lego-desec.env" ''
        DESEC_PROPAGATION_TIMEOUT=700
        DESEC_POLLING_INTERVAL=20
      '';
      extraLegoFlags = [
        "--dns.resolvers"
        "ns.desec.ch:53"
        "--dns.resolvers"
        "ns.desec.cz:53"
        "--dns.resolvers"
        "ns.desec.li:53"
        "--dns.resolvers"
        "ns1.desec.io:53"
        "--dns.resolvers"
        "ns2.desec.org:53"
        "--dns-timeout"
        "30"
        "--dns.propagation-rns"
      ];
      reloadServices = [ (if caddyEdgeEnabled then "caddy.service" else "nginx.service") ];
    };
    certs = {
      "s3.data.${domain.home}" = {
        domain = "s3.data.${domain.home}";
        extraDomainNames = [
          "*.s3.data.${domain.home}"
          "minio.data.${domain.home}"
          "*.s3.mgmt.${domain.home}"
          "minio.mgmt.${domain.home}"
          "s3.mgmt.${domain.home}"
        ];
        group = "nginx";
      };
      "attic.mgmt.${domain.home}" = {
        domain = "attic.mgmt.${domain.home}";
        group = "nginx";
        extraDomainNames = [
          "attic.int.${domain.home}"
        ];
      };
    };
  };

  sops.secrets.desec_api_token = {
    sopsFile = ../../configs/home-ops/shared.sops.yml;
    restartUnits = [ ];
  };

  environment.persistence."/persist".directories = lib.mkIf legacyAcmeEnabled [
    "/var/lib/acme"
  ];
}
