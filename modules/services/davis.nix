{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.services.davis;
  onepassword = config.modules.services.onepassword-systemd-credentials;
  inherit (config.repo.secrets) home-ops;
in
{
  options.modules.services.davis = {
    enable = lib.mkEnableOption "davis";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "dav.example.com";
      description = "The domain to use for the davis";
    };
  };

  disabledModules = [
    "${inputs.nixpkgs}/nixos/modules/services/web-apps/davis.nix"
    "${inputs.nixpkgs-stable}/nixos/modules/services/web-apps/davis.nix"
  ];
  imports = [
    "${inputs.nixpkgs-mine}/nixos/modules/services/web-apps/davis.nix"
  ];
  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = onepassword.enable;
        message = "Davis 1Password credentials require the systemd credential provider.";
      }
    ];

    modules.services.onepassword-systemd-credentials.consumers.davis-env-setup = {
      APP_SECRET = "op://home-ops-prod/davis/APP_SECRET";
      ADMIN_PASSWORD = "op://home-ops-prod/davis/ADMIN_PASSWORD";
    };
    modules.services.caddy.routes.davis = {
      publicHost = cfg.domain;
      handlerConfig = ''
        @davis_well_known path /.well-known/caldav /.well-known/carddav
        redir @davis_well_known https://{http.request.host}/dav/ 302
        @davis_hidden path_regexp davis_hidden \\.ht
        respond @davis_hidden 404
        root * ${config.services.davis.package}/public
        php_fastcgi unix//run/phpfpm/davis.sock {
          env HTTPS on
          env HTTP_X_FORWARDED_PROTO https
          env HTTP_X_FORWARDED_PORT 443
        }
        file_server
      '';
    };
    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/davis"."mountpoint" = config.services.davis.dataDir;
      "rpool/encrypted/safe/svc/davis"."com.sun:auto-snapshot" = "false";
    };
    sops.secrets."davis/APP_SECRET" = {
      sopsFile = ../../configs/home-ops/shared.sops.yml;
      owner = config.services.davis.user;
      inherit (config.services.davis) group;
      mode = "400";
    };
    sops.secrets."davis/ADMIN_PASSWORD" = {
      sopsFile = ../../configs/home-ops/shared.sops.yml;
      owner = config.services.davis.user;
      inherit (config.services.davis) group;
      mode = "400";
    };

    services.davis = {
      enable = true;
      hostname = cfg.domain;
      package = pkgs.davis;
      database = {
        driver = "postgresql";
      };
      mail = {
        inherit (home-ops.mail) dsn;
        inviteFromAddress = home-ops.mail.notificationsFromAddress;
      };
      adminLogin = "admin";
      adminPasswordFile = onepassword.creds.davis-env-setup.ADMIN_PASSWORD;
      appSecretFile = onepassword.creds.davis-env-setup.APP_SECRET;
      config = {
        IMAP_AUTH_URL = home-ops.mail.imapAuthUrlNew;
        IMAP_ENCRYPTION_METHOD = "ssl";
        IMAP_CERTIFICATE_VALIDATION = true;
        AUTH_METHOD = "IMAP";
        IMAP_AUTH_USER_AUTOCREATE = false;
      };
      nginx = null;
      poolConfig = {
        "listen.owner" = "caddy";
        "listen.group" = "caddy";
      };
    };
  };
}
