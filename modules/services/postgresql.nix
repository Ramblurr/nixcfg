{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.services.postgresql;
  backupRole = "databasus_pg_dewey";
  maliMgmtAddress = builtins.head config.site.net.mgmt.hosts4.mali;
  deweyMgmtAddress = builtins.head config.site.net.mgmt.hosts4.${config.networking.hostName};

  serviceDeps = [
    "zfs-datasets.service"
  ];
in
{
  options.modules.services.postgresql = {
    enable = mkEnableOption "postgresql";

    package = mkPackageOption pkgs "postgresql_15" { };

    pgDataDir = mkOption {
      type = types.str;
      example = "/var/lib/postgresql/15";
      default = "/var/lib/postgresql/${lib.versions.major cfg.package.version}";
      description = "The data directory for the PostgreSQL instance, it must be under /var/lib/postgresql";
    };
    extraAuthentication = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Extra authentication configuration for the PostgreSQL instance";
    };
    physicalBackup.enable = lib.mkEnableOption "Databasus physical full/incremental backups";
    ensures = lib.mkOption {
      description = "List of username, database and/or passwords that should be created.";
      type = lib.types.listOf (
        lib.types.submodule {
          options = {
            username = lib.mkOption {
              type = lib.types.str;
              description = "Postgres user name.";
            };
            databases = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              description = "Postgres database names.";
            };
            passwordFile = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              description = "Optional password file for the postgres user. If not given, only peer auth is accepted for this user, otherwise password auth is allowed.";
              default = null;
              example = "/run/secrets/postgresql/password";
            };
          };
        }
      );
      default = [ ];
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = lib.hasPrefix "/var/lib/postgresql" cfg.pgDataDir;
        message = "The PostgreSQL data directory must be under /var/lib/postgresql";
      }
      {
        assertion =
          !cfg.physicalBackup.enable || config.modules.services.onepassword-systemd-credentials.enable;
        message = "PostgreSQL physical backup credentials require the systemd credential provider.";
      }
      {
        assertion =
          !cfg.physicalBackup.enable || !(builtins.elem 5432 config.networking.firewall.allowedTCPPorts);
        message = "PostgreSQL physical backups must not use a globally allowed TCP port.";
      }
    ];
    services.postgresql = {
      enable = true;
      inherit (cfg) package;
      dataDir = cfg.pgDataDir;
      settings = lib.mkIf cfg.physicalBackup.enable {
        wal_level = "replica";
        summarize_wal = "on";
        max_wal_senders = 10;
        max_replication_slots = 10;
      };
      enableTCPIP = true;
      authentication = lib.concatStringsSep "\n" cfg.extraAuthentication;
      ensureDatabases = lib.flatten (map ({ databases, ... }: databases) cfg.ensures);
      ensureUsers = lib.flatten (
        map (
          { username, ... }:
          {
            name = username;
            ensureDBOwnership = true;
            ensureClauses.login = true;
          }
        ) cfg.ensures
      );
    };
    modules.services.postgresql.extraAuthentication = lib.mkIf cfg.physicalBackup.enable (
      lib.mkBefore [
        "host replication ${backupRole} ${maliMgmtAddress}/32 scram-sha-256"
        "host postgres ${backupRole} ${maliMgmtAddress}/32 scram-sha-256"
        "host all ${backupRole} ${maliMgmtAddress}/32 reject"
      ]
    );

    modules.services.onepassword-systemd-credentials.consumers.databasus-pg-dewey-role =
      lib.mkIf cfg.physicalBackup.enable
        {
          POSTGRES_PASSWORD = "op://home-ops-prod/databasus-pg-dewey/password";
        };

    networking.firewall.extraInputRules = lib.mkIf cfg.physicalBackup.enable ''
      iifname "mgmt" ip saddr ${maliMgmtAddress}/32 ip daddr ${deweyMgmtAddress} tcp dport 5432 accept comment "Databasus pg-dewey physical backup"
    '';

    systemd.services.databasus-pg-dewey-role = lib.mkIf cfg.physicalBackup.enable {
      description = "Provision the Databasus replication role for Dewey PostgreSQL";
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
        SELECT 'CREATE ROLE ${backupRole} LOGIN REPLICATION'
        WHERE NOT EXISTS (SELECT FROM pg_roles WHERE rolname = '${backupRole}') \gexec
        ALTER ROLE ${backupRole}
          WITH LOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE REPLICATION NOBYPASSRLS;
        SELECT format('ALTER ROLE %I PASSWORD %L', '${backupRole}', :'password') \gexec
        REVOKE ALL PRIVILEGES ON DATABASE postgres FROM ${backupRole};
        GRANT CONNECT ON DATABASE postgres TO ${backupRole};
        SQL
      '';
      serviceConfig = {
        Type = "oneshot";
        User = "postgres";
        Group = "postgres";
        RemainAfterExit = true;
        ExecStartPre = "${pkgs.coreutils}/bin/test -S /run/postgresql/.s.PGSQL.5432";
      };
    };

    systemd.services.postgresql.postStart =
      let
        prefix = ''
          $PSQL -tA <<'EOF'
            DO $$
            DECLARE password TEXT;
            BEGIN
        '';
        suffix = ''
            END $$;
          EOF
        '';
        exec =
          { username, passwordFile, ... }:
          ''
            password := trim(both from replace(pg_read_file('${passwordFile}'), E'\n', '''));
            EXECUTE format('ALTER ROLE ${username} WITH PASSWORD '''%s''';', password);
          '';
        cfgsWithPasswords = builtins.filter (cfg: cfg.passwordFile != null) cfg.ensures;
      in
      if (builtins.length cfgsWithPasswords) == 0 then
        ""
      else
        prefix + (lib.concatStrings (map exec cfgsWithPasswords)) + suffix;

    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/postgresql"."mountpoint" = "/var/lib/postgresql";
      "rpool/encrypted/safe/svc/postgresql"."com.sun:auto-snapshot" = "false";
      "rpool/encrypted/safe/svc/postgresql"."recordsize" = "16k";
      "rpool/encrypted/safe/svc/postgresql"."primarycache" = "all";
    };
    systemd.tmpfiles.rules = [ "d ${cfg.pgDataDir} 750 postgres postgres" ];

    systemd.services.postgresql = {
      requires = serviceDeps;
      after = serviceDeps;
      bindsTo = [ "zfs-mount.service" ];
      unitConfig = {
        AssertPathIsMountPoint = [ "/var/lib/postgresql" ];
        RequiresMountsFor = [ "/var/lib/postgresql" ];
      };
    };

  };
}
