{
  config,
  pkgs,
  ...
}:
let
  inherit (config.repo.secrets.global) domain email;
in
{
  security.acme = {
    acceptTerms = true;
    defaults = {
      email = email.acme;
      credentialFiles."DESEC_TOKEN_FILE" = config.sops.secrets.desec_api_token.path;
      dnsProvider = "desec";
      environmentFile = pkgs.writeText "lego-desec.env" ''
        DESEC_PROPAGATION_TIMEOUT=600
        DESEC_POLLING_INTERVAL=10
      '';
      extraLegoFlags = [
        "--dns.resolvers"
        "ns.desec.ch:53"
        "--dns.resolvers"
        "ns.desec.cz:53"
        "--dns.resolvers"
        "ns.desec.li:53"
        "--dns.propagation-rns"
        "--dns-timeout"
        "30"
      ];
      reloadServices = [ "nginx.service" ];
    };
  };

  sops.secrets.desec_api_token = {
    sopsFile = ../../configs/home-ops/shared.sops.yml;
    restartUnits = [ ];
  };
  security.acme.certs = {
    #"mali" = {
    #  domain = "mali.int.${domain.home}";
    #  extraDomainNames = [];
    #  group = "nginx";
    #  directory = "/persist/var/lib/acme/mali";
    #};
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

  environment.persistence = {
    "/persist" = {
      directories = [ "/var/lib/acme" ];
    };
  };
}
