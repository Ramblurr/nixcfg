{
  config,
  ...
}:

let
  inherit (config.repo.secrets.global.domain) work;
  domain = work;
  docsDomain = "docs.${domain}";
  deployUser = docsDomain;
  deployUserCfg = config.modules.users.deploy-users.${deployUser};
  sitePath = "/var/lib/static-web/${domain}/docs";
  bootstrapPath = "${sitePath}/bootstrap";
  rootPath = "${sitePath}/current";
in
{
  security.acme.certs.${docsDomain} = {
    domain = docsDomain;
  };

  systemd.tmpfiles.rules = [
    "d '${sitePath}' 0750 ${deployUserCfg.username} nginx - -"
    "d '${bootstrapPath}' 0750 ${deployUserCfg.username} nginx - -"
    "d '${bootstrapPath}/.etc' 0750 ${deployUserCfg.username} nginx - -"
    "d '${bootstrapPath}/.etc/nginx' 0750 ${deployUserCfg.username} nginx - -"
    "f '${bootstrapPath}/.etc/nginx/rewrite.conf' 0640 ${deployUserCfg.username} nginx - -"
    "L '${rootPath}' - - - - ${bootstrapPath}"
  ];
  system.activationScripts.docsSiteNginxBootstrap.text = ''
    install -d -m 0750 -o ${deployUserCfg.username} -g nginx '${sitePath}'
    install -d -m 0750 -o ${deployUserCfg.username} -g nginx '${bootstrapPath}'
    install -d -m 0750 -o ${deployUserCfg.username} -g nginx '${bootstrapPath}/.etc'
    install -d -m 0750 -o ${deployUserCfg.username} -g nginx '${bootstrapPath}/.etc/nginx'

    if [ ! -e '${bootstrapPath}/.etc/nginx/rewrite.conf' ]; then
      install -m 0640 -o ${deployUserCfg.username} -g nginx /dev/null '${bootstrapPath}/.etc/nginx/rewrite.conf'
    fi

    if [ ! -e '${rootPath}' ]; then
      ln -s '${bootstrapPath}' '${rootPath}'
    fi
  '';

  services.nginx.virtualHosts.${docsDomain} = {
    useACMEHost = docsDomain;
    forceSSL = true;
    kTLS = true;
    http3 = true;
    quic = true;
    root = rootPath;
    extraConfig = ''
      add_header Alt-Svc 'h3=":443"; ma=86400';
      etag on;
      if_modified_since exact;
      error_page 404 /404.html;
      include ${rootPath}/.etc/nginx/rewrite.conf;
    '';
    locations."= /.etc/nginx/rewrite.conf" = {
      extraConfig = ''
        deny all;
        return 404;
      '';
    };
    locations."/" = {
      extraConfig = ''
        rewrite ^(.+)/$ $1 break;
        try_files $uri $uri.html $uri/index.html $uri/ =404;
        expires 30m;
        add_header Cache-Control "public, no-transform, max-age=1800, must-revalidate" always;
      '';
    };
    locations."~* \\.(?:html|css|js|json|xml|txt|md)$" = {
      extraConfig = ''
        expires 30m;
        add_header Cache-Control "public, no-transform, max-age=1800, must-revalidate" always;
      '';
    };
    locations."~* \\.(?:png|jpg|jpeg|gif|svg|ico|webp|avif|woff|woff2|ttf|otf|eot)$" = {
      extraConfig = ''
        expires 30d;
        add_header Cache-Control "public, no-transform, max-age=2592000, must-revalidate" always;
      '';
    };
  };
}
