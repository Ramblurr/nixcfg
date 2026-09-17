{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.services.bookorbit;
  libraryPath = "/mnt/mali/${cfg.mediaNfsShare}/books";
  stateDir = "/var/lib/bookorbit";
  dataset = "rpool/encrypted/safe/svc/bookorbit";
in
{
  disabledModules = [
    "${inputs.nixpkgs}/nixos/modules/services/web-apps/bookorbit.nix"
    "${inputs.nixpkgs-stable}/nixos/modules/services/web-apps/bookorbit.nix"
  ];
  imports = [ "${inputs.nixpkgs-mine}/nixos/modules/services/web-apps/bookorbit.nix" ];

  options.modules.services.bookorbit = {
    enable = lib.mkEnableOption "BookOrbit";
    disableLocalAuth = lib.mkEnableOption "OIDC-only login after provider setup and administrator linking";
    domain = lib.mkOption { type = lib.types.str; };
    ports.http = lib.mkOption { type = lib.types.port; };
    mediaNfsShare = lib.mkOption { type = lib.types.str; };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.modules.services.onepassword-systemd-credentials.enable;
        message = "BookOrbit requires the 1Password systemd credential provider.";
      }
    ];

    services.bookorbit = {
      enable = true;
      environment = {
        HOST = "127.0.0.1";
        PORT = cfg.ports.http;
        APP_URL = "https://${cfg.domain}";
        APP_DATA_PATH = stateDir;
        LIBRARY_BROWSE_ROOT = libraryPath;
        TRUST_PROXY = "127.0.0.1";
        DISABLE_LOCAL_AUTH = lib.boolToString cfg.disableLocalAuth;
      };
    };

    modules.zfs.datasets = {
      properties.${dataset}.mountpoint = stateDir;
      services.${dataset} = [ "bookorbit" ];
    };

    modules.services.onepassword-systemd-credentials.consumers.bookorbit = {
      JWT_SECRET = "op://home-ops-prod/bookorbit/JWT_SECRET";
      SETUP_BOOTSTRAP_TOKEN = "op://home-ops-prod/bookorbit/SETUP_BOOTSTRAP_TOKEN";
      BOOK_REQUEST_ENCRYPTION_KEY = "op://home-ops-prod/bookorbit/BOOK_REQUEST_ENCRYPTION_KEY";
    };

    systemd.services.bookorbit = {
      unitConfig.RequiresMountsFor = [
        stateDir
        libraryPath
      ];
      serviceConfig = {
        SupplementaryGroups = [ "media" ];
        ReadWritePaths = [ libraryPath ];
        # Override upstream's private-file mask for shared library writes.
        UMask = lib.mkForce "0002";
        ExecStart = lib.mkForce (
          lib.getExe (
            pkgs.writeShellScriptBin "bookorbit-start" ''
              set -eu
              export JWT_SECRET="$(cat "$CREDENTIALS_DIRECTORY/JWT_SECRET")"
              export SETUP_BOOTSTRAP_TOKEN="$(cat "$CREDENTIALS_DIRECTORY/SETUP_BOOTSTRAP_TOKEN")"
              export BOOK_REQUEST_ENCRYPTION_KEY="$(cat "$CREDENTIALS_DIRECTORY/BOOK_REQUEST_ENCRYPTION_KEY")"
              exec ${lib.getExe config.services.bookorbit.package}
            ''
          )
        );
        Restart = "on-failure";
        RestartSec = "5s";
      };
    };

    modules.services.caddy.routes.bookorbit = {
      publicHost = cfg.domain;
      upstream = "http://127.0.0.1:${toString cfg.ports.http}";
    };

    site.gatus.endpoints = [
      {
        name = "BookOrbit";
        group = config.site.gatus.groups.media;
        url = "https://${cfg.domain}/api/v1/health";
      }
    ];
  };
}
