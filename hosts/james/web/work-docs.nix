{
  config,
  pkgs,
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
  hookSocketPath = config.hosts.james.webhooks.socketPath;
  docsWebhookSecretFile = config.sops.secrets.webhook-github-docs-secret.path;
  webhookService = "webhook-work-site.service";
  docsHookScript = pkgs.writeScript "work-docs-hook.sh" ''
    #!${pkgs.bash}/bin/bash
    set -euo pipefail
    repo="''${1:-unknown}"
    ref="''${2:-unknown}"
    echo "work-docs webhook matched for $repo at $ref"
  '';
in
{
  sops.secrets.webhook-github-docs-secret = {
    restartUnits = [ webhookService ];
  };

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
    locations."/_deploy" = {
      proxyPass = "http://unix:${hookSocketPath}";
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

  hosts.james.webhooks.hooks = {
    update-docs = {
      secretsFile = docsWebhookSecretFile;
      execute-command = docsHookScript;
      command-working-directory = sitePath;
      response-message = "docs webhook accepted";
      trigger-rule = {
        and = [
          {
            match = {
              type = "value";
              value = "push";
              parameter = {
                source = "header";
                name = "X-GitHub-Event";
              };
            };
          }
          {
            match = {
              type = "regex";
              regex = "^refs/(heads/main|heads/v[0-9].*|tags/v.*)$";
              parameter = {
                source = "payload";
                name = "ref";
              };
            };
          }
          {
            match = {
              type = "regex";
              regex = "^outskirtslabs/.+";
              parameter = {
                source = "payload";
                name = "repository.full_name";
              };
            };
          }
        ];
      };
      pass-arguments-to-command = [
        {
          source = "payload";
          name = "repository.full_name";
        }
        {
          source = "payload";
          name = "ref";
        }
      ];
    };
  };
}
