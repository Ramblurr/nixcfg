{
  lib,
  pkgs,
  config,
  ...
}: {
  security.acme = {
    acceptTerms = true;
    defaults = {
      email = "acme@outskirtslabs.com";
      dnsProvider = "cloudflare";
      credentialsFile = config.sops.templates.acme-credentials.path;
      extraLegoFlags = ["--dns.resolvers=1.1.1.1:53"];
    };
  };

  sops.secrets."acmeSecrets/cloudflareDnsToken" = {
    sopsFile = ./secrets.sops.yaml;
    restartUnits = [];
  };

  sops.secrets."acmeSecrets/cloudflareZoneToken" = {
    sopsFile = ./secrets.sops.yaml;
    restartUnits = [];
  };

  sops.templates.acme-credentials.content = ''
    CF_DNS_API_TOKEN=${config.sops.placeholder."acmeSecrets/cloudflareDnsToken"}
    CF_ZONE_API_TOKEN=${config.sops.placeholder."acmeSecrets/cloudflareZoneToken"}
  '';
  security.acme.certs = {
    #"mali" = {
    #  domain = "mali.int.socozy.casa";
    #  extraDomainNames = [];
    #  group = "nginx";
    #  directory = "/persist/var/lib/acme/mali";
    #};
    "s3.data.socozy.casa" = {
      domain = "s3.data.socozy.casa";
      extraDomainNames = ["*.s3.data.socozy.casa" "minio.data.socozy.casa"];
      postRun = "systemctl reload nginx.service";
      group = "nginx";
    };
  };

  environment.persistence = {
    "/persist" = {
      directories = [
        "/var/lib/acme"
      ];
    };
  };
}
