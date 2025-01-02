{
  lib,
  pkgs,
  config,
  globals,
  ...
}:
let
  inherit (globals) domain;
in
{
  age.secrets.acmeCredentialsFile = {
    rekeyFile = ./secrets/acmeCredentialsFile.age;
  };
  security.acme = {
    acceptTerms = true;
    defaults = {
      email = globals.email.acme;
      dnsProvider = "cloudflare";
      credentialsFile = config.age.secrets.acmeCredentialsFile.path;
      extraLegoFlags = [ "--dns.resolvers=1.1.1.1:53" ];
    };
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
      postRun = "systemctl reload nginx.service";
      group = "nginx";
    };
    "attic.mgmt.${domain.home}" = {
      domain = "attic.mgmt.${domain.home}";
      postRun = "systemctl reload nginx.service";
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
