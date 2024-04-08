{
  lib,
  pkgs,
  config,
  ...
}:
let
  inherit (config.repo.secrets.global) domain;
in
{
  security.acme = {
    acceptTerms = true;
    defaults = {
      email = domain.acme.email;
      dnsProvider = "cloudflare";
      credentialsFile = config.sops.templates.acme-credentials.path;
      extraLegoFlags = [ "--dns.resolvers=1.1.1.1:53" ];
    };
  };

  sops.secrets."acmeSecrets/cloudflareDnsToken" = {
    sopsFile = ./secrets.sops.yaml;
    restartUnits = [ ];
  };

  sops.templates.acme-credentials.content = ''
    CF_DNS_API_TOKEN=${config.sops.placeholder."acmeSecrets/cloudflareDnsToken"}
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
      postRun = "systemctl reload nginx.service";
      group = "nginx";
    };
    "attic.mgmt.${domain.home}" = {
      domain = "attic.mgmt.${domain.home}";
      postRun = "systemctl reload nginx.service";
      group = "nginx";
    };
  };

  environment.persistence = {
    "/persist" = {
      directories = [ "/var/lib/acme" ];
    };
  };
}
