{
  config,
  lib,
  ...
}:

let
  inherit (config.repo.secrets.global.domain)
    home
    personal2
    ;
  inherit (config.repo.secrets.local) atprotoDid;
  codeLink = config.repo.secrets.global.code;
  domain = personal2;
  homeDirectory = "/var/lib/${personal2}";
  #runtimeDirectory = "${config.modules.users.deploy-users.${personal2}.runtimeDirectory}";
  socketPath = "${homeDirectory}/.run/site.sock";
in
{
  modules.users.deploy-users.${personal2} = {
    username = personal2;
    uid = 993;
    gid = 991;
    inherit homeDirectory;
    homeDirectoryOnZfs.enable = true;
    homeDirectoryOnZfs.datasetName = "rpool/encrypted/safe/svc/${personal2}";
  };

  security.acme.certs.${domain} = {
    domain = "${domain}";
    extraDomainNames = [
      "www.${domain}"
      "code.${domain}"
    ];
  };
  users.users.nginx.extraGroups = [ personal2 ];
  systemd.services.nginx.serviceConfig = {
    ProtectHome = "tmpfs";
    BindReadOnlyPaths = [
      "/var/lib/${personal2}/.run"
    ];
  };
  services.nginx.virtualHosts.${domain} = {
    serverAliases = [ "www.${domain}" ];
    useACMEHost = domain;
    forceSSL = true;
    kTLS = true;
    http3 = true;
    http2 = false;
    quic = true;
    locations."= /.well-known/carddav".extraConfig = ''
      return 301 https://dav.${home}/dav/;
    '';
    locations."= /.well-known/caldav".extraConfig = ''
      return 301 https://dav.${home}/dav/;
    '';
    locations."= /.well-known/atproto-did".extraConfig = ''
      add_header Content-Type text/plain;
      return 200 '${atprotoDid}';
    '';
    locations."/" = {
      recommendedProxySettings = true;
      proxyWebsockets = true;
      proxyPass = "http://unix:${socketPath}";
      extraConfig = ''
        ${lib.optionalString true ''
          add_header Alt-Svc 'h3=":443"; ma=86400';
        ''}
      '';
    };
  };

  services.nginx.virtualHosts."code.${domain}" = {
    useACMEHost = domain;
    forceSSL = true;
    kTLS = true;
    http3 = true;
    http2 = false;
    quic = true;
    locations."/" = {
      return = "302 ${codeLink}";
    };
  };
}
