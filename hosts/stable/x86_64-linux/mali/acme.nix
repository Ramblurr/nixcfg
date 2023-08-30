{
  lib,
  pkgs,
  ...
}: let
  sopsFile = builtins.getEnv "SOPS_SECRETS_FILE";
  acmeSecrets = (lib.importJSON sopsFile).acme_secrets;

  creds = pkgs.writeTextFile {
    name = "cloudflare.env";
    text = ''
      CF_DNS_API_TOKEN=${acmeSecrets.cloudflareDnsToken}
      CF_ZONE_API_TOKEN=${acmeSecrets.cloudflareZoneToken}
    '';
  };
  email = acmeSecrets.email;

  extraLegoFlags = ["--dns.resolvers=1.1.1.1:53"];
  certStanzas =
    lib.mapAttrsToList (name: value: {
      inherit name;
      group = value.group;
      dnsProvider = value.dnsProvider;
      extraDomainNames = value.extraDomainNames;
    })
    acmeSecrets.certs;
in {
  security.acme.defaults.email = email;
  security.acme.acceptTerms = true;

  security.acme.certs = lib.listToAttrs (builtins.map (x: {
      name = x.name;
      value =
        {
          group = x.group;
          extraDomainNames = x.extraDomainNames;
        }
        // {
          credentialsFile = "${creds}";
          dnsProvider = "cloudflare";
          email = email;
          inherit extraLegoFlags;
        };
    })
    certStanzas);

  environment.persistence = {
    "/persist" = {
      directories = [
        "/var/lib/acme"
      ];
    };
  };
}
