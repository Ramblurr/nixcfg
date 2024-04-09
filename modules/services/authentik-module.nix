{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.services.authentik;
  databaseActuallyCreateLocally =
    cfg.database.createLocally && cfg.database.host == "/run/postgresql";
  hostWithPort = h: p: "${h}:${toString p}";
  shouldImportSSL = cfg.ssl.cert != null && cfg.ssl.key != null && cfg.ssl.name != null;
  authentikBaseService = {
    after = [ "network.target" ] ++ lib.optional databaseActuallyCreateLocally "postgresql.service";
    wantedBy = [ "multi-user.target" ];
    path = [ cfg.package ];
    environment =
      let
        listenAddress = hostWithPort cfg.listen.address;
      in
      {
        AUTHENTIK_REDIS__HOST = cfg.redis.host;
        AUTHENTIK_REDIS__PORT = toString cfg.redis.port;

        AUTHENTIK_POSTGRESQL__HOST = cfg.database.host;
        AUTHENTIK_POSTGRESQL__USER = cfg.database.user;
        AUTHENTIK_POSTGRESQL__NAME = cfg.database.name;

        AUTHENTIK_LISTEN__HTTP = listenAddress cfg.listen.http;
        AUTHENTIK_LISTEN__HTTPS = listenAddress cfg.listen.https;

        # disable outbound connections
        AUTHENTIK_DISABLE_UPDATE_CHECK = "true";
        AUTHENTIK_ERROR_REPORTING__ENABLED = "false";
        AUTHENTIK_DISABLE_STARTUP_ANALYTICS = "true";
        AUTHENTIK_AVATARS = "initials";

        AUTHENTIK_LOG_LEVEL = cfg.logLevel;
      }
      // lib.optionalAttrs (!databaseActuallyCreateLocally) {
        AUTHENTIK_POSTGRESQL__PORT = toString cfg.database.port;
      };
    serviceConfig = {
      User = cfg.user;
      Group = cfg.group;
      EnvironmentFile = cfg.environmentFile;
      WorkingDirectory = cfg.package;
      DynamicUser = true;
      RuntimeDirectory = "authentik";
      NoNewPrivileges = true;
      PrivateTmp = true;
      ProtectHome = true;
      ProtectSystem = "strict";
      PrivateDevices = true;
      ProtectKernelTunables = true;
      ProtectKernelModules = true;
      ProtectControlGroups = true;
      SystemCallFilter = "~@cpu-emulation @keyring @module @obsolete @raw-io @reboot @swap @sync";
      ConfigurationDirectory = "authentik";
      StateDirectoryMode = "0750";
      LoadCredential = lib.optional (
        cfg.adminPasswordFile != null
      ) "ADMIN_PASSWORD:${cfg.adminPasswordFile}";
    };
  };
in
{
  options.services.authentik = {
    enable = lib.mkEnableOption "Enables Authentik service";

    package = lib.mkPackageOption pkgs "authentik" { };

    user = lib.mkOption {
      description = lib.mdDoc ''
        User under which authentik runs.
      '';
      type = lib.types.str;
      default = "authentik";
    };

    group = lib.mkOption {
      description = lib.mdDoc ''
        Group under which authentik runs.
      '';
      type = lib.types.str;
      default = "authentik";
    };

    adminPasswordFile = lib.mkOption {
      description = lib.mdDoc "Default admin password. Only read on first startup.";
      type = lib.types.nullOr lib.types.path;
      default = null;
    };

    logLevel = lib.mkOption {
      description = lib.mdDoc "Log level for the server and worker containers. Setting the log level to trace will include sensitive details in logs, so it shouldn't be used in most cases.";
      type = lib.types.enum [
        "trace"
        "debug"
        "info"
        "warning"
        "error"
      ];
      default = "info";
    };

    listen = lib.mkOption {
      description = lib.mdDoc "Listen ports";
      default = { };
      type = lib.types.submodule {
        options = {
          http = lib.mkOption {
            description = lib.mdDoc "HTTP port.";
            type = lib.types.port;
            default = 9000;
          };
          https = lib.mkOption {
            description = lib.mdDoc "HTTPS port.";
            type = lib.types.port;
            default = 9443;
          };
          address = lib.mkOption {
            description = lib.mdDoc "Address to listen on.";
            type = lib.types.str;
            default = "127.0.0.1";
          };
        };
      };
    };
    redis = {
      createLocally = lib.mkOption {
        description = lib.mdDoc "Configure local Redis server for Authentik.";
        type = lib.types.bool;
        default = true;
      };

      host = lib.mkOption {
        description = lib.mdDoc "Redis host.";
        type = lib.types.str;
        default = "127.0.0.1";
      };

      port = lib.mkOption {
        description = lib.mdDoc "Redis port.";
        type = lib.types.port;
        default = 31637;
      };
    };
    ssl = {
      cert = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
      };

      key = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
      };

      name = lib.mkOption {
        type = lib.types.str;
        default = "SSL from NixOS";
      };
    };

    environmentFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      example = "/var/lib/authentik/secrets/db-password";
      description = lib.mdDoc ''
        Environment variables including :

        -  Secret key used for cookie signing and unique user IDs, don't change this after the first install.
      '';
    };
    database = {
      createLocally = lib.mkOption {
        description = lib.mdDoc "Configure local PostgreSQL database server for authentik.";
        type = lib.types.bool;
        default = true;
      };

      host = lib.mkOption {
        type = lib.types.str;
        default = "/run/postgresql";
        example = "192.168.23.42";
        description = lib.mdDoc "Database host address or unix socket.";
      };

      port = lib.mkOption {
        type = lib.types.nullOr lib.types.port;
        default = if cfg.database.createLocally then null else 5432;
        defaultText = lib.literalExpression ''
          if config.database.createLocally
          then null
          else 5432
        '';
        description = lib.mdDoc "Database host port.";
      };

      name = lib.mkOption {
        type = lib.types.str;
        default = "authentik";
        description = lib.mdDoc "Database name.";
      };

      user = lib.mkOption {
        type = lib.types.str;
        default = "authentik";
        description = lib.mdDoc "Database user.";
      };
    };
    outposts = lib.mkOption {
      type = lib.types.submodule {
        options = {
          ldap = lib.mkOption {
            type = lib.types.submodule {
              options = {
                enable = lib.mkEnableOption (lib.mdDoc "the authentik ldap outpost");
                package = lib.mkOption {
                  type = lib.types.path;
                  default = pkgs.authentik-outposts.ldap;
                };
                host = lib.mkOption {
                  type = lib.types.str;
                  default = "https://127.0.0.1:9443";
                };
                insecure = lib.mkOption {
                  type = lib.types.bool;
                  default = false;
                };
                environmentFile = lib.mkOption {
                  type = lib.types.nullOr lib.types.path;
                  default = null;
                  example = "/var/lib/authentik-ldap/secrets/env";
                  description = lib.mdDoc ''
                    Environment variables including :
                    -  API TOKEN
                  '';
                };
                listen = lib.mkOption {
                  description = lib.mdDoc "Listen ports";
                  default = { };
                  type = lib.types.submodule {
                    options = {
                      ldap = lib.mkOption {
                        description = lib.mdDoc "LDAP port.";
                        type = lib.types.port;
                        default = 3389;
                      };
                      ldaps = lib.mkOption {
                        description = lib.mdDoc "LDAPS port.";
                        type = lib.types.port;
                        default = 6636;
                      };
                      address = lib.mkOption {
                        description = lib.mdDoc "Address to listen on.";
                        type = lib.types.str;
                        default = "0.0.0.0";
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
      default = {
        ldap = {
          enable = false;
        };
      };
    };
  };
  config = lib.mkIf cfg.enable {

    users.users = (
      lib.mkIf (cfg.user == "authentik") {
        authentik = {
          isSystemUser = true;
          home = cfg.package;
          group = cfg.group;
        };
      }
    );
    users.groups = lib.mkIf (cfg.user == "authentik" && cfg.group == "authentik") {
      ${cfg.group} = { };
    };
    services.postgresql = lib.mkIf databaseActuallyCreateLocally {
      enable = true;
      ensureUsers = [
        {
          name = cfg.database.name;
          ensureDBOwnership = true;
        }
      ];
      ensureDatabases = [ cfg.database.name ];
    };
    services.redis.servers.authentik =
      lib.mkIf (cfg.redis.createLocally && cfg.redis.host == "127.0.0.1")
        {
          enable = true;
          port = cfg.redis.port;
          bind = "127.0.0.1";
        };
    systemd.services.authentik-server = authentikBaseService // {
      serviceConfig = authentikBaseService.serviceConfig;
      script = ''
        ${lib.optionalString (cfg.adminPasswordFile != null) ''
          export AUTHENTIK_BOOTSTRAP_PASSWORD "''${CREDENTIALS_DIRECTORY}/ADMIN_PASSWORD")
        ''}
          ${cfg.package}/bin/ak server
      '';
    };
    systemd.services.authentik-worker = authentikBaseService // {
      serviceConfig = authentikBaseService.serviceConfig // {
        ExecStart = "${cfg.package}/bin/ak worker";
      };
    };
    systemd.services.authentik-ssl-import = lib.mkIf shouldImportSSL authentikBaseService // {
      before = [ "authentik-server.service" ];
      serviceConfig = authentikBaseService.serviceConfig // {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${cfg.package}/bin/ak import_certificate --name \"${cfg.ssl.name}\" --certificate \"${cfg.ssl.cert}\" --private-key \"${cfg.ssl.key}\"";
      };
    };
    systemd.services.authentik-ldap-outpost =
      let
        ldapCfg = cfg.outposts.ldap;
      in
      lib.mkIf ldapCfg.enable (
        authentikBaseService
        // {
          description = "authentik ldap outpost";
          environment =
            let
              listenAddress = hostWithPort ldapCfg.listen.address;
            in
            {
              AUTHENTIK_HOST = ldapCfg.host;
              AUTHENTIK_LISTEN__LDAP = listenAddress ldapCfg.listen.ldap;
              AUTHENTIK_LISTEN__LDAPS = listenAddress ldapCfg.listen.ldaps;
            }
            // lib.optionalAttrs ldapCfg.insecure { AUTHENTIK_INSECURE = "true"; };
          serviceConfig = authentikBaseService.serviceConfig // {
            ExecStart = "${cfg.outposts.ldap.package}/bin/ldap";
            EnvironmentFile = ldapCfg.environmentFile;
          };
        }
      );
  };
}
