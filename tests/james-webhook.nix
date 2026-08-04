{
  inputs,
  pkgs,
}:
let
  lib = inputs.nixpkgs.lib;
  testSecretFile = "/run/secrets/webhook-test-secret";
  testHookId = "test-non-deploy";
  testSocketPath = "/build/github-work-site-${testHookId}.sock";
  markerScript = pkgs.writeShellScript "webhook-test-deploy" ''
    touch "$WEBHOOK_TEST_MARKER"
  '';

  sopsOptions =
    { lib, ... }:
    {
      options.sops.secrets = lib.mkOption {
        type = lib.types.attrsOf (
          lib.types.submodule (
            { name, ... }:
            {
              options = {
                path = lib.mkOption {
                  type = lib.types.str;
                  default = "/run/secrets/${name}";
                };
                restartUnits = lib.mkOption {
                  type = lib.types.listOf lib.types.str;
                  default = [ ];
                };
              };
            }
          )
        );
        default = { };
      };
    };

  evaluated = lib.nixosSystem {
    modules = [
      ../hosts/james/web/hook.nix
      ../hosts/james/web/work.nix
      sopsOptions
      (
        { lib, ... }:
        {
          options.repo.secrets = lib.mkOption { type = lib.types.attrs; };

          config = {
            nixpkgs.pkgs = pkgs;
            system.stateVersion = "25.11";
            services.nginx.enable = true;
            repo.secrets.global = {
              codeWork = "https://code.example.test";
              git.work = "https://git.example.test/work.git";
              domain = {
                home = "home.example.test";
                work = "work.example.test";
                work2 = "work2.example.test";
              };
            };
            hosts.james.webhooks.hooks.${testHookId} = {
              secretsFile = testSecretFile;
              socketPath = testSocketPath;
              execute-command = markerScript;
              response-message = "accepted";
              trigger-rule.match = {
                type = "value";
                value = "refs/heads/main";
                parameter = {
                  source = "payload";
                  name = "ref";
                };
              };
            };
          };
        }
      )
    ];
  };

  cfg = evaluated.config;
  hookId = "deploy-work.example.test";
  serviceName = cfg.hosts.james.webhooks.hookServiceNames.${hookId};
  service = cfg.systemd.services.${lib.removeSuffix ".service" serviceName};
  socketDirectory = "/var/lib/static-web/work.example.test/.run";
  socketPath = "${socketDirectory}/github-work-site-${hookId}.sock";
  socketDirectoryRule = "d '${socketDirectory}' 0750 nginx nginx - -";
  proxyPass = cfg.services.nginx.virtualHosts."work.example.test".locations."/_deploy".proxyPass;

  testServiceName = cfg.hosts.james.webhooks.hookServiceNames.${testHookId};
  testService = cfg.systemd.services.${lib.removeSuffix ".service" testServiceName};
  testServiceScript = pkgs.writeShellScript "webhook-test-service" testService.script;
  testLoadCredential = testService.serviceConfig.LoadCredential;
  testLoadCredentialEntry =
    if builtins.isList testLoadCredential then builtins.head testLoadCredential else testLoadCredential;
  credentialName = builtins.head (lib.splitString ":" testLoadCredentialEntry);
  workLoadCredential = service.serviceConfig.LoadCredential;
  workLoadCredentialEntry =
    if builtins.isList workLoadCredential then builtins.head workLoadCredential else workLoadCredential;
in
assert cfg.hosts.james.webhooks.hookSocketPaths.${hookId} == socketPath;
assert proxyPass == "http://unix:${socketPath}";
assert builtins.elem socketDirectoryRule cfg.systemd.tmpfiles.rules;
assert service.serviceConfig.User == "nginx";
assert service.serviceConfig.Group == "nginx";
assert service.serviceConfig.UMask == "0007";
assert service.serviceConfig.Restart == "on-failure";
assert service.serviceConfig.RestartSec == "5s";
assert service.startLimitIntervalSec == 300;
assert service.startLimitBurst == 10;
assert builtins.elem "sops-install-secrets.service" service.after;
assert builtins.elem "sops-install-secrets.service" service.requires;
assert builtins.elem "systemd-tmpfiles-setup.service" service.after;
assert builtins.elem "systemd-tmpfiles-setup.service" service.requires;
assert lib.hasInfix "-socket ${socketPath}" service.script;
assert !(lib.hasInfix "/run/nginx" service.script);
assert lib.hasPrefix "WEBHOOK_SECRET_FILE_" credentialName;
assert lib.hasSuffix ":/run/secrets/webhook-github-work-secret" workLoadCredentialEntry;
pkgs.runCommand "james-webhook-module-test"
  {
    nativeBuildInputs = [
      pkgs.curl
      pkgs.openssl
    ];
  }
  ''
    set -euo pipefail

    credentials="$PWD/credentials"
    marker="$PWD/deployed"
    mkdir "$credentials"
    printf %s test-secret > "$credentials/${credentialName}"
    export CREDENTIALS_DIRECTORY="$credentials"
    export WEBHOOK_TEST_MARKER="$marker"

    ${testServiceScript} > "$PWD/webhook.log" 2>&1 &
    webhook_pid=$!
    cleanup() {
      kill "$webhook_pid" 2>/dev/null || true
      wait "$webhook_pid" 2>/dev/null || true
    }
    trap cleanup EXIT

    for _ in $(seq 1 100); do
      test -S ${testSocketPath} && break
      sleep 0.1
    done
    test -S ${testSocketPath}

    main_body="$PWD/main.json"
    non_main_body="$PWD/non-main.json"
    printf '{"ref":"refs/heads/main"}' > "$main_body"
    printf '{"ref":"refs/heads/testing"}' > "$non_main_body"

    send_request() {
      local body=$1
      local signature=$2
      local expected=$3
      local response="$PWD/response"
      local status
      status=$(${pkgs.curl}/bin/curl --silent --show-error \
        --unix-socket ${testSocketPath} \
        -o "$response" \
        -w '%{http_code}' \
        -H 'Content-Type: application/json' \
        -H "X-Hub-Signature: $signature" \
        --data-binary "@$body" \
        http://localhost/_deploy/${testHookId})
      test "$status" = 200
      test "$(cat "$response")" = "$expected"
    }

    send_request \
      "$main_body" \
      sha1=0000000000000000000000000000000000000000 \
      'Hook rules were not satisfied.'
    sleep 0.2
    test ! -e "$marker"

    non_main_signature=$(${pkgs.openssl}/bin/openssl dgst -sha1 -hmac test-secret "$non_main_body" | awk '{print $NF}')
    send_request "$non_main_body" "sha1=$non_main_signature" 'Hook rules were not satisfied.'
    sleep 0.2
    test ! -e "$marker"

    main_signature=$(${pkgs.openssl}/bin/openssl dgst -sha1 -hmac test-secret "$main_body" | awk '{print $NF}')
    send_request "$main_body" "sha1=$main_signature" accepted
    for _ in $(seq 1 50); do
      test -e "$marker" && break
      sleep 0.1
    done
    test -e "$marker"

    for forbidden in \
      test-secret \
      0000000000000000000000000000000000000000 \
      refs/heads/testing
    do
      if grep -Fq -- "$forbidden" "$PWD/webhook.log"; then
        exit 1
      fi
    done

    touch "$out"
  ''
