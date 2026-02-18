{
  config,
  pkgs,
  ...
}:

let
  inherit (config.repo.secrets.global)
    codeWork
    git
    ;
  inherit (config.repo.secrets.global.domain) home work work2;
  domain = work;
  sitePath = "/var/lib/static-web/${domain}";
  rootPath = "${sitePath}/www";
  socketPath = "/var/run/nginx/github-work-hook.sock";
  webhookService = "webhook-work-site.service";
in
{
  security.acme.certs.${domain} = {
    domain = "${domain}";
    extraDomainNames = [
      "www.${domain}"
      "${work2}"
      "www.${work2}"
      "code.${domain}"
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

  services.nginx.virtualHosts."code.${domain}" = {
    useACMEHost = domain;
    forceSSL = true;
    kTLS = true;
    http3 = true;
    quic = true;
    locations."/" = {
      return = "302 ${codeWork}";
    };
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
    root = rootPath;
    extraConfig = ''
      add_header Alt-Svc 'h3=":443"; ma=86400';
    '';
    locations."/_deploy" = {
      proxyPass = "http://unix:${socketPath}";
    };
    locations."= /.well-known/carddav".extraConfig = ''
      return 301 https://dav.${home}/dav/;
    '';
    locations."= /.well-known/caldav".extraConfig = ''
      return 301 https://dav.${home}/dav/;
    '';
    locations."= /.well-known/matrix/server".extraConfig =
      let
        server = {
          "m.server" = "matrix.${domain}:443";
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
            "base_url" = "https://matrix.${domain}";
          };
        };
      in
      ''
        add_header Content-Type application/json;
        add_header Access-Control-Allow-Origin *;
        return 200 '${builtins.toJSON client}';
      '';
  };

  sops.secrets.webhook-github-work-secret = {
    restartUnits = [ webhookService ];
  };

  hosts.james.webhooks = {
    enable = true;
    serviceName = "work-site";
    inherit socketPath;
    urlPrefix = "_deploy";
    secretsFile = config.sops.secrets.webhook-github-work-secret.path;
    hooks = {
      "deploy-${domain}" = {
        execute-command = pkgs.writeScript "work-deploy.sh" ''
          #!${pkgs.bash}/bin/bash
          set -euo pipefail
          TEMP_DIR=$(${pkgs.coreutils}/bin/mktemp -d)
          trap 'rm -rf "$TEMP_DIR"' EXIT
          ${pkgs.git}/bin/git clone ${git.work} "$TEMP_DIR"
          OLD_PATH="${rootPath}.$(date +%Y%m%d%H%M%S)"
          mv ${rootPath} "$OLD_PATH"
          mv "$TEMP_DIR/site/" ${rootPath}

          # clean up old backups (keep last 5)
          ${pkgs.findutils}/bin/find . -name "$(basename ${rootPath}).*" -type d | sort | head -n -5 | ${pkgs.findutils}/bin/xargs -r rm -rf
        '';
        command-working-directory = sitePath;
        response-message = "Deploying...";
        trigger-rule = {
          match = {
            type = "value";
            value = "refs/heads/main";
            parameter = {
              source = "payload";
              name = "ref";
            };
          };
        };
      };
    };
  };

}
