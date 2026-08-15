{
  config,
  pkgs,
  ...
}:

let
  inherit (config.repo.secrets.global.domain) work;
  domain = work;
  hookId = "update-docs";
  docsDomain = "docs.${domain}";
  deployUser = docsDomain;
  deployUserCfg = config.modules.users.deploy-users.${deployUser};
  caddyGroup = config.services.caddy.group;
  sitePath = "/var/lib/static-web/${domain}/docs";
  bootstrapPath = "${sitePath}/bootstrap";
  rootPath = "${sitePath}/current";
  docsHookSocketDir = "${sitePath}/.run";
  docsHookSocketPath = "${docsHookSocketDir}/github-docs-hook.sock";
  docsWebhookSecretFile = config.sops.secrets.webhook-github-docs-secret.path;
  docsDispatchTokenFile = config.sops.secrets.github_token_docs_trigger_pat.path;
  webhookService = config.hosts.james.webhooks.hookServiceNames.${hookId};
  docsHookScript = pkgs.writeScript "work-docs-hook.sh" ''
    #!${pkgs.bash}/bin/bash
    set -euo pipefail
    repo="''${1:-}"
    ref="''${2:-}"
    dispatch_repo="outskirtslabs/docs"
    dispatch_workflow="update-docs.yml"
    dispatch_ref="main"
    dispatch_token_file="${docsDispatchTokenFile}"

    if [[ -z "$repo" || -z "$ref" ]]; then
      echo "missing required webhook arguments (repo/ref)"
      exit 1
    fi

    if [[ -z "''${GH_TOKEN:-}" && -n "$dispatch_token_file" && -f "$dispatch_token_file" ]]; then
      export GH_TOKEN="$(${pkgs.coreutils}/bin/cat "$dispatch_token_file")"
      export GITHUB_TOKEN="$GH_TOKEN"
    fi

    if [[ -z "''${GH_TOKEN:-}" ]]; then
      echo "GH_TOKEN is not set; cannot dispatch $dispatch_repo/$dispatch_workflow"
      exit 1
    fi

    echo "dispatching $dispatch_repo/$dispatch_workflow from source $repo at $ref"
    ${pkgs.gh}/bin/gh workflow run "$dispatch_workflow" \
      --repo "$dispatch_repo" \
      --ref "$dispatch_ref"
    echo "dispatch submitted"
  '';
in
{
  sops.secrets.github_token_docs_trigger_pat = {
    owner = deployUserCfg.username;
    group = deployUserCfg.username;
    mode = "0400";
    restartUnits = [ webhookService ];
  };

  sops.secrets.webhook-github-docs-secret = {
    restartUnits = [ webhookService ];
  };

  systemd.tmpfiles.rules = [
    "d '${sitePath}' 0750 ${deployUserCfg.username} ${caddyGroup} - -"
    "d '${docsHookSocketDir}' 0750 ${deployUserCfg.username} ${caddyGroup} - -"
    "d '${bootstrapPath}' 0750 ${deployUserCfg.username} ${caddyGroup} - -"
    "d '${bootstrapPath}/.etc' 0750 ${deployUserCfg.username} ${caddyGroup} - -"
    "d '${bootstrapPath}/.etc/nginx' 0750 ${deployUserCfg.username} ${caddyGroup} - -"
    "f '${bootstrapPath}/.etc/nginx/rewrite.conf' 0640 ${deployUserCfg.username} ${caddyGroup} - -"
  ];
  system.activationScripts.docsSiteBootstrap.text = ''
    install -d -m 0750 -o ${deployUserCfg.username} -g ${caddyGroup} '${sitePath}'
    install -d -m 0750 -o ${deployUserCfg.username} -g ${caddyGroup} '${docsHookSocketDir}'
    install -d -m 0750 -o ${deployUserCfg.username} -g ${caddyGroup} '${bootstrapPath}'
    install -d -m 0750 -o ${deployUserCfg.username} -g ${caddyGroup} '${bootstrapPath}/.etc'
    install -d -m 0750 -o ${deployUserCfg.username} -g ${caddyGroup} '${bootstrapPath}/.etc/nginx'

    if [ ! -e '${bootstrapPath}/.etc/nginx/rewrite.conf' ]; then
      install -m 0640 -o ${deployUserCfg.username} -g ${caddyGroup} /dev/null '${bootstrapPath}/.etc/nginx/rewrite.conf'
    fi

    if [ ! -e '${rootPath}' ]; then
      ln -s '${bootstrapPath}' '${rootPath}'
    fi
  '';

  hosts.james.webhooks.hooks = {
    ${hookId} = {
      secretsFile = docsWebhookSecretFile;
      user = deployUserCfg.username;
      group = caddyGroup;
      socketPath = docsHookSocketPath;
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
