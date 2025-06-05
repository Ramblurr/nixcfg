{
  config,
  lib,
  pkgs,
  ...
}:

let

  inherit (config.repo.secrets.global) domain;
  work = domain.work;
  domains = [
    domain.personal2
    domain.personal4
    domain.personal5
    domain.personal6
  ];
  certFor = domain: {
    ${domain} = {
      domain = "${domain}";
      extraDomainNames = [
        "www.${domain}"
      ];
    };
  };
  vhostFor = domain: {
    ${domain} = {
      serverAliases = [ "www.${domain}" ];
      useACMEHost = domain;
      forceSSL = true;
      kTLS = true;
      http3 = true;
      quic = true;
      globalRedirect = work;
    };
  };
in
{
  security.acme.certs = lib.mkMerge (map certFor domains);
  services.nginx.virtualHosts = lib.mkMerge (map vhostFor domains);
}
