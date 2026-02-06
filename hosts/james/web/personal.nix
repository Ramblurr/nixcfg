{
  config,
  ...
}:

let
  inherit (config.repo.secrets.global.domain)
    home
    work
    personal1
    ;
  codeLink = config.repo.secrets.global.code;
  domain = personal1;
in
{
  security.acme.certs.${domain} = {
    domain = "${domain}";
    extraDomainNames = [
      "www.${domain}"
      "code.${domain}"
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

  services.nginx.virtualHosts."code.${domain}" = {
    useACMEHost = domain;
    forceSSL = true;
    kTLS = true;
    http3 = true;
    quic = true;
    locations."/" = {
      return = "302 ${codeLink}";
    };
  };
}
