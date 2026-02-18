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
  deployUser = personal2;
  deployUserCfg = config.modules.users.deploy-users.${deployUser};
  domain = deployUser;
  homeDirectory = deployUserCfg.homeDirectory;
  #runtimeDirectory = "${deployUserCfg.runtimeDirectory}";
  socketPath = "${homeDirectory}/.run/site.sock";
in
{
  security.acme.certs.${domain} = {
    domain = "${domain}";
    extraDomainNames = [
      "www.${domain}"
      "code.${domain}"
    ];
  };
  users.users.nginx.extraGroups = [ deployUser ];
  systemd.services.nginx.serviceConfig = {
    ProtectHome = "tmpfs";
    BindReadOnlyPaths = [
      "${homeDirectory}/.run"
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
