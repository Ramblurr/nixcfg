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
  davisUser = home-ops.users.davis;
  davisGroup = home-ops.groups.davis;
  backupRole = "databasus_davis";
  databaseName = "davis";
  maliMgmtAddress = builtins.head config.site.net.mgmt.hosts4.mali;
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
      {
        assertion = !(builtins.elem 5432 config.networking.firewall.allowedTCPPorts);
        message = "Davis PostgreSQL must not be exposed through globally allowed TCP port 5432.";
      }
    ];

    modules.services.onepassword-systemd-credentials.consumers.davis-env-setup = {
      APP_SECRET = "op://home-ops-prod/davis/APP_SECRET";
      ADMIN_PASSWORD = "op://home-ops-prod/davis/ADMIN_PASSWORD";
    };
    modules.services.onepassword-systemd-credentials.consumers.databasus-davis-role = {
      POSTGRES_PASSWORD = "op://home-ops-prod/databasus-davis/password";
    };

    modules.services.postgresql.extraAuthentication = [
      "host ${databaseName} ${backupRole} ${maliMgmtAddress}/32 scram-sha-256"
      "host all ${backupRole} ${maliMgmtAddress}/32 reject"
    ];

    systemd.services.phpfpm-davis = {
      requires = [ "postgresql.service" ];
      after = [ "postgresql.service" ];
    };
    systemd.services.davis-db-migrate = {
      requires = [ "postgresql.service" ];
      after = [ "postgresql.service" ];
    };

    systemd.services.databasus-davis-role = {
      description = "Provision the Databasus read-only role for Davis";
      wantedBy = [ "multi-user.target" ];
      requires = [ "postgresql.service" ];
      after = [ "postgresql.service" ];
      script = ''
        ${config.services.postgresql.package}/bin/psql \
          --no-psqlrc \
          --quiet \
          --set ON_ERROR_STOP=1 \
          --dbname postgres <<'SQL'
        \set password `cat "$CREDENTIALS_DIRECTORY/POSTGRES_PASSWORD"`
        SELECT 'CREATE ROLE ${backupRole} LOGIN'
        WHERE NOT EXISTS (SELECT FROM pg_roles WHERE rolname = '${backupRole}') \gexec
        ALTER ROLE ${backupRole}
          WITH LOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION NOBYPASSRLS;
        SELECT format('ALTER ROLE %I PASSWORD %L', '${backupRole}', :'password') \gexec
        REVOKE ALL PRIVILEGES ON DATABASE ${databaseName} FROM ${backupRole};
        REVOKE TEMP ON DATABASE ${databaseName} FROM PUBLIC;
        GRANT CONNECT ON DATABASE ${databaseName} TO ${backupRole};
        SQL

        ${config.services.postgresql.package}/bin/psql \
          --no-psqlrc \
          --quiet \
          --set ON_ERROR_STOP=1 \
          --dbname ${databaseName} <<'SQL'
        REVOKE ALL PRIVILEGES ON SCHEMA public FROM ${backupRole};
        GRANT USAGE ON SCHEMA public TO ${backupRole};
        REVOKE ALL PRIVILEGES ON ALL TABLES IN SCHEMA public FROM ${backupRole};
        GRANT SELECT ON ALL TABLES IN SCHEMA public TO ${backupRole};
        REVOKE ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public FROM ${backupRole};
        GRANT SELECT ON ALL SEQUENCES IN SCHEMA public TO ${backupRole};
        ALTER DEFAULT PRIVILEGES FOR ROLE davis IN SCHEMA public
          REVOKE ALL PRIVILEGES ON TABLES FROM ${backupRole};
        ALTER DEFAULT PRIVILEGES FOR ROLE davis IN SCHEMA public
          GRANT SELECT ON TABLES TO ${backupRole};
        ALTER DEFAULT PRIVILEGES FOR ROLE davis IN SCHEMA public
          REVOKE ALL PRIVILEGES ON SEQUENCES FROM ${backupRole};
        ALTER DEFAULT PRIVILEGES FOR ROLE davis IN SCHEMA public
          GRANT SELECT ON SEQUENCES TO ${backupRole};
        SQL
      '';
      serviceConfig = {
        Type = "oneshot";
        User = "postgres";
        Group = "postgres";
        RemainAfterExit = true;
      };
    };
    site.gatus.endpoints = [
      {
        name = "Davis";
        group = config.site.gatus.groups.home;
        url = "https://${cfg.domain}/";
      }
    ];

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
    users.users.${davisUser.name} = {
      inherit (davisUser) name uid isSystemUser;
      group = davisGroup.name;
    };
    users.groups.${davisGroup.name} = {
      inherit (davisGroup) gid;
    };

    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/davis"."mountpoint" = config.services.davis.dataDir;
      "rpool/encrypted/safe/svc/davis"."com.sun:auto-snapshot" = "false";
    };
    services.davis = {
      enable = true;
      user = davisUser.name;
      group = davisGroup.name;
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
