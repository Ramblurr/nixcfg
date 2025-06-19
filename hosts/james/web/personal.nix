{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.repo.secrets.global.domain)
    home
    work
    personal1
    ;
  domain = personal1;
in
{
  security.acme.certs.${domain} = {
    domain = "${domain}";
    extraDomainNames = [
      "www.${domain}"
    ];
  };

  services.nginx.virtualHosts.${domain} = {
    serverAliases = [ "www.${domain}" ];
    useACMEHost = domain;
    forceSSL = true;
    kTLS = true;
    http3 = true;
    quic = true;
    globalRedirect = work;
    locations."= /.well-known/carddav".extraConfig = ''
      return 301 https://dav.${home}/dav/;
    '';
    locations."= /.well-known/caldav".extraConfig = ''
      return 301 https://dav.${home}/dav/;
    '';
  };
}
