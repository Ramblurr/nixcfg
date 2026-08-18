{
  config,
  pkgs,
  ...
}:

let
  inherit (config.repo.secrets.global) git;
  inherit (config.repo.secrets.global.domain) work;
  domain = work;
  hookId = "deploy-${domain}";
  sitePath = "/var/lib/static-web/${domain}";
  hookSocketDirectory = "${sitePath}/.run";
  rootPath = "${sitePath}/www";
  webhookService = config.hosts.james.webhooks.hookServiceNames.${hookId};
  caddyUser = config.services.caddy.user;
  caddyGroup = config.services.caddy.group;
in
{
  systemd.tmpfiles.rules = [
    "d '${sitePath}' 0750 ${caddyUser} ${caddyGroup} - -"
    "d '${hookSocketDirectory}' 0750 ${caddyUser} ${caddyGroup} - -"
    "Z '${sitePath}/www*' - ${caddyUser} ${caddyGroup} - -"
  ];

  sops.secrets.webhook-github-work-secret = {
    restartUnits = [ webhookService ];
  };

  hosts.james.webhooks = {
    enable = true;
    serviceName = "work-site";
    urlPrefix = "_deploy";
    socketDirectory = hookSocketDirectory;
    user = caddyUser;
    group = caddyGroup;
    secretsFile = config.sops.secrets.webhook-github-work-secret.path;
    hooks = {
      ${hookId} = {
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
