{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.repo.secrets.global.domain) home work work2;
  domain = work;
in
{
  security.acme.certs.${domain} = {
    domain = "${domain}";
    extraDomainNames = [
      "www.${domain}"
      "${work2}"
      "www.${work2}"
    ];
  };

  services.nginx.virtualHosts.${work2} = {
    serverAliases = [
      "www.${work2}"
    ];
    useACMEHost = domain;
    forceSSL = true;
    kTLS = true;
    http3 = true;
    quic = true;
    globalRedirect = domain;
  };
  services.nginx.virtualHosts.${domain} = {
    serverAliases = [
      "www.${domain}"
    ];
    useACMEHost = domain;
    forceSSL = true;
    kTLS = true;
    http3 = true;
    quic = true;
    root = "/var/lib/static-web/${domain}";
    extraConfig = ''
      add_header Alt-Svc 'h3=":443"; ma=86400';
    '';
    locations."= /.well-known/carddav".extraConfig = ''
      return 301 https://dav.${home}/dav/;
    '';
    locations."= /.well-known/caldav".extraConfig = ''
      return 301 https://dav.${home}/dav/;
    '';
    locations."= /.well-known/matrix/server".extraConfig =
      let
        server = {
          "m.server" = "matrix.${domain}.org:443";
        };
      in
      ''
        add_header Content-Type application/json;
        return 200 '${builtins.toJSON server}';
      '';
    locations."= /.well-known/matrix/client".extraConfig =
      let
        client = {
          "m.homeserver" = {
            "base_url" = "https://matrix.${domain}.org";
          };
        };
      in
      ''
        add_header Content-Type application/json;
        add_header Access-Control-Allow-Origin *;
        return 200 '${builtins.toJSON client}';
      '';
  };

}
