{
  lib,
  pkgs,
  config,
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
      dnsProvider = "bunny";
      dnsPropagationCheck = false;
      credentialsFile = config.sops.templates.acme-credentials.path;
      reloadServices = [ "nginx.service" ];
    };
  };

  sops.secrets.bunnyApiKey = {
    sopsFile = ../../configs/home-ops/shared.sops.yml;
    restartUnits = [ ];
  };

  sops.templates.acme-credentials.content = ''
    BUNNY_API_KEY=${config.sops.placeholder.bunnyApiKey}
  '';
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
