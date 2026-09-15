{
  inputs,
  pkgs,
}:
let
  lib = inputs.nixpkgs.lib;

  secretFile = pkgs.writeText "paperless-caddy-secrets.yaml" "{}\n";
  testOptions =
    { lib, ... }:
    {
      options = {
        repo.secrets = lib.mkOption { type = lib.types.attrs; };
        modules.zfs.datasets.properties = lib.mkOption {
          type = lib.types.attrsOf (lib.types.attrsOf lib.types.str);
          default = { };
        };
        site.net.svc.hosts4 = lib.mkOption { type = lib.types.attrsOf (lib.types.listOf lib.types.str); };
      };
    };

  mkSystem =
    oidc:
    lib.nixosSystem {
      modules = [
        inputs.sops-nix.nixosModules.sops
        inputs.impermanence.nixosModules.impermanence
        ../modules/services/caddy.nix
        ../modules/services/onepassword-systemd-credentials.nix
        ../modules/site/gatus.nix
        ../modules/services/paperless.nix
        ../modules/services/paperless-gpt.nix
        testOptions
        {
          nixpkgs.pkgs = pkgs;
          sops.defaultSopsFile = secretFile;
          sops.age.keyFile = "/tmp/age-key.txt";
          modules.services.onepassword-systemd-credentials = {
            enable = true;
            connectHost = "http://127.0.0.1:8080";
          };
          boot.loader.grub.devices = [ "nodev" ];
          fileSystems."/" = {
            device = "none";
            fsType = "tmpfs";
          };
          modules.services.caddy.edge = {
            certificateDomains = [ "example.test" ];
            acmeEmail = "admin@example.test";
          };
          system.stateVersion = "26.05";
          repo.secrets.global.nodes.mali.dataCIDR = "192.0.2.1";
          site.net.svc.hosts4.quine = [ "192.0.2.2" ];
          modules.services.paperless = {
            enable = true;
            domain = "paperless.example.test";
            ports.http = 28981;
            nfsShare = "paperless";
            user = {
              name = "paperless";
              uid = 991;
            };
            group = {
              name = "paperless";
              gid = 991;
            };
            inherit oidc;
          };
        }
      ];
    };

  disabled = (mkSystem { enable = false; }).config;
  compatibility =
    (mkSystem {
      enable = true;
      mode = "compatibility";
    }).config;
  enforced =
    (mkSystem {
      enable = true;
      mode = "enforced";
    }).config;
  sidecar =
    ((mkSystem { enable = false; }).extendModules {
      modules = [
        {
          modules.services.paperless-gpt = {
            enable = true;
            domain = "paperless-gpt.example.test";
          };
          modules.services.caddy.auth.issuerURL = "https://id.example.test";
        }
      ];
    }).config;
  sidecarService = sidecar.systemd.services.paperless-gpt;

  compatibilitySettings = compatibility.services.paperless.settings;
  enforcedSettings = enforced.services.paperless.settings;
  paperlessCredentials = {
    admin-password = "op://home-ops-prod/paperless/admin-password";
    mistral-api-key = "op://home-ops-prod/paperless/mistral-api-key";
    oidc-provider = "op://home-ops-prod/paperless/oidc-provider";
  };
  provider = compatibility.modules.services.onepassword-systemd-credentials;
  setupService = compatibility.systemd.services.paperless-secrets-setup;
  secretKeyService = compatibility.systemd.services.paperless-secret-key;
  webService = compatibility.systemd.services.paperless-web;
  postgresDependentServices = map (name: compatibility.systemd.services.${name}) [
    "paperless-consumer"
    "paperless-scheduler"
    "paperless-task-queue"
    "paperless-web"
  ];
in
assert !(builtins.hasAttr "PAPERLESS_APPS" disabled.services.paperless.settings);
assert
  disabled.modules.services.onepassword-systemd-credentials.consumers.paperless-secrets-setup == {
    admin-password = "op://home-ops-prod/paperless/admin-password";
    mistral-api-key = "op://home-ops-prod/paperless/mistral-api-key";
  };
assert !(builtins.hasAttr "paperless/adminPassword" disabled.sops.secrets);
assert !(builtins.hasAttr "paperless/oidcProvider" disabled.sops.secrets);
assert disabled.sops.templates == { };
assert !(builtins.hasAttr "PAPERLESS_ENABLE_HTTP_REMOTE_USER" disabled.services.paperless.settings);
assert
  !(builtins.hasAttr "PAPERLESS_HTTP_REMOTE_USER_HEADER_NAME" disabled.services.paperless.settings);
assert compatibilitySettings.PAPERLESS_APPS == "allauth.socialaccount.providers.openid_connect";
assert compatibilitySettings.PAPERLESS_ACCOUNT_DEFAULT_HTTP_PROTOCOL == "https";
assert compatibilitySettings.PAPERLESS_AI_LLM_BACKEND == "openai-like";
assert compatibilitySettings.PAPERLESS_AI_LLM_MODEL == "mistral-small-latest";
assert compatibilitySettings.PAPERLESS_AI_LLM_CONTEXT_SIZE == 16384;
assert compatibilitySettings.PAPERLESS_AI_LLM_ENDPOINT == "https://api.mistral.ai/v1";
assert !(builtins.hasAttr "PAPERLESS_AI_LLM_API_KEY" compatibilitySettings);
assert disabled.services.paperless.environmentFile == "/run/paperless-secrets/paperless.env";
assert compatibilitySettings.PAPERLESS_AI_LLM_EMBEDDING_BACKEND == "openai-like";
assert compatibilitySettings.PAPERLESS_AI_LLM_EMBEDDING_MODEL == "mistral-embed";
assert compatibilitySettings.PAPERLESS_AI_LLM_EMBEDDING_ENDPOINT == "https://api.mistral.ai/v1";
assert !compatibilitySettings.PAPERLESS_AI_LLM_ALLOW_INTERNAL_ENDPOINTS;
assert !compatibilitySettings.PAPERLESS_SOCIALACCOUNT_ALLOW_SIGNUPS;
assert !compatibilitySettings.PAPERLESS_SOCIAL_AUTO_SIGNUP;
assert !compatibilitySettings.PAPERLESS_SOCIAL_ACCOUNT_SYNC_GROUPS;
assert !compatibilitySettings.PAPERLESS_DISABLE_REGULAR_LOGIN;
assert !compatibilitySettings.PAPERLESS_REDIRECT_LOGIN_TO_SSO;
assert enforcedSettings.PAPERLESS_DISABLE_REGULAR_LOGIN;
assert enforcedSettings.PAPERLESS_REDIRECT_LOGIN_TO_SSO;
assert compatibility.services.paperless.environmentFile == "/run/paperless-secrets/paperless.env";
assert compatibility.services.paperless.passwordFile == "/run/paperless-secrets/admin-password";
assert provider.consumers.paperless-secrets-setup == paperlessCredentials;
assert builtins.elem "paperless-web.service" setupService.requiredBy;
assert builtins.elem "paperless-secrets-setup.service" secretKeyService.requires;
assert builtins.elem "paperless-secrets-setup.service" secretKeyService.after;
assert builtins.elem "onepassword-credential-provider.socket" setupService.requires;
assert builtins.elem "paperless-secrets-setup.service" webService.requires;
assert builtins.elem "paperless-secrets-setup.service" webService.after;
assert lib.all (
  service: builtins.elem "postgresql.service" service.requires
) postgresDependentServices;
assert lib.all (
  service: builtins.elem "postgresql.service" service.after
) postgresDependentServices;
assert !(builtins.hasAttr "paperless-copy-password" compatibility.systemd.services);
assert builtins.elem "/run/paperless-secrets/paperless.env" (
  lib.toList webService.serviceConfig.EnvironmentFile
);
assert !(builtins.hasAttr "paperless/adminPassword" compatibility.sops.secrets);
assert !(builtins.hasAttr "paperless/oidcProvider" compatibility.sops.secrets);
assert compatibility.sops.templates == { };
assert lib.hasInfix "/var/lib/paperless/nixos-paperless-secret-key.env"
  compatibility.systemd.services.paperless-secret-key.script;
assert compatibility.services.caddy.enable;
assert compatibility.modules.services.caddy.routes.paperless.publicHost == "paperless.example.test";
assert !(builtins.hasAttr "paperless-gpt" disabled.systemd.services);
assert sidecarService.environment.LISTEN_SOCKET == "/run/paperless-gpt/http.sock";
assert !(builtins.hasAttr "LISTEN_INTERFACE" sidecarService.environment);
assert sidecarService.environment.PAPERLESS_BASE_URL == "http://127.0.0.1:28981";
assert sidecarService.environment.OCR_PROVIDER == "mistral_ocr";
assert sidecarService.environment.OCR_PROCESS_MODE == "whole_pdf";
assert sidecarService.environment.MISTRAL_MODEL == "mistral-ocr-latest";
assert sidecarService.environment.PDF_REPLACE == "false";
assert !(builtins.hasAttr "MISTRAL_API_KEY" sidecarService.environment);
assert !(builtins.hasAttr "PAPERLESS_API_TOKEN" sidecarService.environment);
assert sidecarService.serviceConfig.DynamicUser;
assert sidecarService.serviceConfig.UMask == "0007";
assert sidecarService.serviceConfig.RuntimeDirectoryMode == "0750";
assert sidecarService.serviceConfig.StateDirectoryMode == "0700";
assert builtins.elem "paperless-gpt-proxy" sidecar.users.users.caddy.extraGroups;
assert
  sidecar.modules.services.onepassword-systemd-credentials.consumers.paperless-gpt == {
    paperless-api-token = "op://home-ops-prod/paperless-gpt/api-token";
    mistral-api-key = "op://home-ops-prod/paperless/mistral-api-key";
  };
assert
  sidecar.modules.services.caddy.protectedRoutes.paperless-gpt.upstream
  == "unix//run/paperless-gpt/http.sock";
assert sidecar.modules.services.caddy.protectedRoutes.paperless-gpt.bypassPathPrefixes == [ ];
assert builtins.all (assertion: assertion.assertion) sidecar.assertions;
pkgs.runCommand "paperless-oidc-module-test" { } ''
  touch "$out"
''
