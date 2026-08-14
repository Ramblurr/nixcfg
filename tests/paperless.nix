{
  inputs,
  pkgs,
}:
let
  lib = inputs.nixpkgs.lib;

  testOptions =
    { lib, ... }:
    {
      options = {
        repo.secrets = lib.mkOption { type = lib.types.attrs; };
        modules.zfs.datasets.properties = lib.mkOption {
          type = lib.types.attrsOf (lib.types.attrsOf lib.types.str);
          default = { };
        };
        modules.services.ingress = {
          domains = lib.mkOption {
            type = lib.types.attrs;
            default = { };
          };
          virtualHosts = lib.mkOption {
            type = lib.types.attrs;
            default = { };
          };
        };
      };
    };

  mkSystem =
    oidc:
    lib.nixosSystem {
      modules = [
        inputs.sops-nix.nixosModules.sops
        ../modules/services/paperless.nix
        testOptions
        {
          nixpkgs.pkgs = pkgs;
          system.stateVersion = "26.05";
          repo.secrets.global.nodes.mali.dataCIDR = "192.0.2.1";
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
            ingress.domain = "example.test";
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

  compatibilitySettings = compatibility.services.paperless.settings;
  enforcedSettings = enforced.services.paperless.settings;
  oidcSecretName = "paperless/oidcProvider";
  oidcTemplate = compatibility.sops.templates."paperless-oidc.env";
  webService = compatibility.systemd.services.paperless-web;
in
assert !(builtins.hasAttr "PAPERLESS_APPS" disabled.services.paperless.settings);
assert !(builtins.hasAttr oidcSecretName disabled.sops.secrets);
assert !(builtins.hasAttr "paperless-oidc.env" disabled.sops.templates);
assert !(builtins.hasAttr "PAPERLESS_ENABLE_HTTP_REMOTE_USER" disabled.services.paperless.settings);
assert
  !(builtins.hasAttr "PAPERLESS_HTTP_REMOTE_USER_HEADER_NAME" disabled.services.paperless.settings);
assert compatibilitySettings.PAPERLESS_APPS == "allauth.socialaccount.providers.openid_connect";
assert compatibilitySettings.PAPERLESS_ACCOUNT_DEFAULT_HTTP_PROTOCOL == "https";
assert !compatibilitySettings.PAPERLESS_SOCIALACCOUNT_ALLOW_SIGNUPS;
assert !compatibilitySettings.PAPERLESS_SOCIAL_AUTO_SIGNUP;
assert !compatibilitySettings.PAPERLESS_SOCIAL_ACCOUNT_SYNC_GROUPS;
assert !compatibilitySettings.PAPERLESS_DISABLE_REGULAR_LOGIN;
assert !compatibilitySettings.PAPERLESS_REDIRECT_LOGIN_TO_SSO;
assert enforcedSettings.PAPERLESS_DISABLE_REGULAR_LOGIN;
assert enforcedSettings.PAPERLESS_REDIRECT_LOGIN_TO_SSO;
assert compatibility.services.paperless.environmentFile == oidcTemplate.path;
assert oidcTemplate.owner == "paperless";
assert oidcTemplate.group == "paperless";
assert oidcTemplate.mode == "0400";
assert oidcTemplate.restartUnits == [ "paperless-web.service" ];
assert
  oidcTemplate.content == ''
    PAPERLESS_SOCIALACCOUNT_PROVIDERS='${compatibility.sops.placeholder.${oidcSecretName}}'
  '';
assert compatibility.sops.secrets.${oidcSecretName}.owner == "paperless";
assert compatibility.sops.secrets.${oidcSecretName}.mode == "0400";
assert builtins.elem "sops-install-secrets.service" webService.requires;
assert builtins.elem "sops-install-secrets.service" webService.after;
assert webService.serviceConfig.EnvironmentFile == oidcTemplate.path;
assert lib.hasInfix "/var/lib/paperless/nixos-paperless-secret-key" webService.script;
assert !compatibility.modules.services.ingress.virtualHosts."paperless.example.test".forwardAuth;
pkgs.runCommand "paperless-oidc-module-test" { } ''
  touch "$out"
''
