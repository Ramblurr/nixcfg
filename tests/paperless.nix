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

  compatibilitySettings = compatibility.services.paperless.settings;
  enforcedSettings = enforced.services.paperless.settings;
  paperlessCredentials = {
    admin-password = "op://home-ops-prod/paperless/admin-password";
    oidc-provider = "op://home-ops-prod/paperless/oidc-provider";
  };
  provider = compatibility.modules.services.onepassword-systemd-credentials;
  setupService = compatibility.systemd.services.paperless-secrets-setup;
  webService = compatibility.systemd.services.paperless-web;
in
assert !(builtins.hasAttr "PAPERLESS_APPS" disabled.services.paperless.settings);
assert
  disabled.modules.services.onepassword-systemd-credentials.consumers.paperless-secrets-setup == {
    admin-password = "op://home-ops-prod/paperless/admin-password";
  };
assert !(builtins.hasAttr "paperless/adminPassword" disabled.sops.secrets);
assert !(builtins.hasAttr "paperless/oidcProvider" disabled.sops.secrets);
assert disabled.sops.templates == { };
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
assert compatibility.services.paperless.environmentFile == "/run/paperless-secrets/oidc.env";
assert compatibility.services.paperless.passwordFile == "/run/paperless-secrets/admin-password";
assert provider.consumers.paperless-secrets-setup == paperlessCredentials;
assert builtins.elem "paperless-web.service" setupService.requiredBy;
assert builtins.elem "onepassword-credential-provider.socket" setupService.requires;
assert builtins.elem "paperless-secrets-setup.service" webService.requires;
assert builtins.elem "paperless-secrets-setup.service" webService.after;
assert webService.serviceConfig.EnvironmentFile == "/run/paperless-secrets/oidc.env";
assert !(builtins.hasAttr "paperless/adminPassword" compatibility.sops.secrets);
assert !(builtins.hasAttr "paperless/oidcProvider" compatibility.sops.secrets);
assert compatibility.sops.templates == { };
assert lib.hasInfix "/var/lib/paperless/nixos-paperless-secret-key" webService.script;
assert compatibility.services.caddy.enable;
assert compatibility.modules.services.caddy.routes.paperless.publicHost == "paperless.example.test";
pkgs.runCommand "paperless-oidc-module-test" { } ''
  touch "$out"
''
