{
  config,
  lib,
  utils,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.paperless;
  onepassword = config.modules.services.onepassword-systemd-credentials;
  localPath = "/mnt/mali/${cfg.nfsShare}";
  paperlessPasswordFile = "/run/paperless-secrets/admin-password";
  paperlessOidcEnvironmentFile = "/run/paperless-secrets/oidc.env";
  paperlessServices = [
    "paperless-consumer"
    "paperless-copy-password"
    "paperless-scheduler"
    "paperless-task-queue"
    "paperless-web"
  ];
  serviceDeps = [ "${utils.escapeSystemdPath localPath}.mount" ];
  oidcDeps = [ "paperless-secrets-setup.service" ];
in
{
  options.modules.services.paperless = {
    enable = lib.mkEnableOption "paperless";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "paperless.example.com";
      description = "The domain to use for the paperless";
    };

    oidc = {
      enable = lib.mkEnableOption "native OpenID Connect authentication";
      mode = lib.mkOption {
        type = lib.types.enum [
          "compatibility"
          "enforced"
        ];
        default = "compatibility";
        description = "Whether to retain regular frontend login or redirect all frontend logins to OIDC";
      };
    };

    ports = {
      http = lib.mkOption {
        type = lib.types.port;
        description = "The HTTP port to use for paperless";
      };
    };
    nfsShare = lib.mkOption { type = lib.types.str; };
    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption { type = lib.types.unspecified; };
  };

  config = lib.mkIf cfg.enable {

    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/paperless"."mountpoint" = config.services.paperless.dataDir;
      "rpool/encrypted/safe/svc/paperless"."com.sun:auto-snapshot" = "false";
    };

    assertions = [
      {
        assertion = onepassword.enable;
        message = "Paperless credentials require the 1Password systemd credential provider.";
      }
    ];

    modules.services.onepassword-systemd-credentials.consumers.paperless-secrets-setup = {
      admin-password = "op://home-ops-prod/paperless/admin-password";
    }
    // lib.optionalAttrs cfg.oidc.enable {
      oidc-provider = "op://home-ops-prod/paperless/oidc-provider";
    };

    systemd.services = lib.mkMerge [
      {
        paperless-secrets-setup = {
          description = "Prepare Paperless files from 1Password credentials";
          before = map (service: "${service}.service") paperlessServices;
          requiredBy = map (service: "${service}.service") paperlessServices;
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            RuntimeDirectory = "paperless-secrets";
            UMask = "0077";
          };
          script = ''
            install -m0400 -o ${config.services.paperless.user} -g ${cfg.group.name} \
              "$CREDENTIALS_DIRECTORY/admin-password" ${paperlessPasswordFile}
            ${lib.optionalString cfg.oidc.enable ''
              printf "PAPERLESS_SOCIALACCOUNT_PROVIDERS='%s'\n" \
                "$(cat "$CREDENTIALS_DIRECTORY/oidc-provider")" > ${paperlessOidcEnvironmentFile}
              chown ${config.services.paperless.user}:${cfg.group.name} ${paperlessOidcEnvironmentFile}
            ''}
          '';
        };
      }
      (lib.genAttrs paperlessServices (_: {
        after = serviceDeps ++ oidcDeps;
        bindsTo = serviceDeps;
        requires = oidcDeps;
      }))
    ];

    services.postgresql = {
      ensureDatabases = [ "paperless" ];
      ensureUsers = [
        {
          inherit (cfg.user) name;
          ensureDBOwnership = true;
        }
      ];
    };

    users.users.${cfg.user.name} = {
      inherit (cfg.user) name;
      uid = lib.mkForce cfg.user.uid;
      isSystemUser = true;
      group = lib.mkForce cfg.group.name;
    };

    users.groups.${cfg.group.name} = {
      inherit (cfg.group) name;
      gid = lib.mkForce cfg.group.gid;
    };

    fileSystems."${localPath}" = {
      device = "${lib.my.cidrToIp config.repo.secrets.global.nodes.mali.dataCIDR}:/mnt/${cfg.nfsShare}";
      fsType = "nfs";
    };

    systemd.tmpfiles.rules =
      let
        inherit (config.services) paperless;
      in
      [
        "d '${paperless.dataDir}' - ${paperless.user} ${config.users.users.${paperless.user}.group} - -"
      ];
    systemd.tmpfiles.settings."10-paperless" = lib.mkForce { };
    services.paperless = {
      enable = true;
      package = pkgs.paperless-ngx;
      mediaDir = "${localPath}/media";
      consumptionDir = "${localPath}/consume";
      passwordFile = paperlessPasswordFile;
      port = cfg.ports.http;
      user = cfg.user.name;
      environmentFile = lib.mkIf cfg.oidc.enable paperlessOidcEnvironmentFile;
      settings = {
        PAPERLESS_EXPORT_DIR = "${localPath}/export";
        PAPERLESS_DBENGINE = "postgresql";
        PAPERLESS_DBHOST = "/run/postgresql";
        PAPERLESS_DBNAME = "paperless";
        PAPERLESS_CONSUMER_POLLING = 60;
        PAPERLESS_CONSUMER_RECURSIVE = true;
        PAPERLESS_CONSUMER_SUBDIRS_AS_TAGS = true;
        PAPERLESS_OCR_LANGUAGE = "deu+eng";
        PAPERLESS_PORT = 8080;
        PAPERLESS_TASK_WORKERS = 2;
        PAPERLESS_TIKA_ENABLED = 0;
        PAPERLESS_TIKA_GOTENBERG_ENDPOINT = "http://localhost:3000";
        PAPERLESS_TIKA_ENDPOINT = "http://localhost:9998";
        PAPERLESS_TIME_ZONE = "Europe/Berlin";
        PAPERLESS_FILENAME_FORMAT = "{created_year}/{created_year}-{created_month}-{created_day} {title}";
        PAPERLESS_FILENAME_DATE_ORDER = "YMD";
        PAPERLESS_URL = "https://${cfg.domain}";
        PAPERLESS_OCR_MAX_IMAGE_PIXELS = 956000000;
        PAPERLESS_ACCOUNT_ALLOW_SIGNUPS = "false";
      }
      // lib.optionalAttrs cfg.oidc.enable {
        PAPERLESS_APPS = "allauth.socialaccount.providers.openid_connect";
        PAPERLESS_ACCOUNT_DEFAULT_HTTP_PROTOCOL = "https";
        PAPERLESS_SOCIALACCOUNT_ALLOW_SIGNUPS = false;
        PAPERLESS_SOCIAL_AUTO_SIGNUP = false;
        PAPERLESS_SOCIAL_ACCOUNT_SYNC_GROUPS = false;
        PAPERLESS_DISABLE_REGULAR_LOGIN = cfg.oidc.mode == "enforced";
        PAPERLESS_REDIRECT_LOGIN_TO_SSO = cfg.oidc.mode == "enforced";
      };
    };

    modules.services.caddy.routes.paperless = {
      publicHost = cfg.domain;
      upstream = "http://127.0.0.1:${toString cfg.ports.http}";
    };
  };
}
