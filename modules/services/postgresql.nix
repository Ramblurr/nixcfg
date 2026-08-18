{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.services.postgresql;

  serviceDeps = [
    "var-lib-postgresql.mount"
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
      default = config.services.postgresql.dataDir;
      description = "The data directory for the PostgreSQL instance, it must be under /var/lib/postgresql";
    };
    extraAuthentication = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Extra authentication configuration for the PostgreSQL instance";
    };
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
    ];
    services.postgresql = {
      enable = true;
      inherit (cfg) package;
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

    systemd.services.postgresql.requires = serviceDeps;
    systemd.services.postgresql.wants = serviceDeps;

  };
}
