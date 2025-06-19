{
  config,
  lib,
  pkgs,
  ...
}:

let

  inherit (config.repo.secrets.global) domain;
  inherit (config.repo.secrets.local) atprotoDid;
  work = domain.work;
  domains = [
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
  vhostFor = d: {
    ${d} = {
      serverAliases = [ "www.${d}" ];
      useACMEHost = d;
      forceSSL = true;
      kTLS = true;
      http3 = true;
      quic = true;
      globalRedirect = work;

      locations."= /.well-known/atproto-did".extraConfig = lib.mkIf (d == domain.personal2) ''
        add_header Content-Type text/plain;
        return 200 '${atprotoDid}';
      '';
    };
  };
in
{
  security.acme.certs = lib.mkMerge (map certFor domains);
  services.nginx.virtualHosts = lib.mkMerge (map vhostFor domains);

}
