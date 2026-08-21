{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.services.invoiceninja;
  onepassword = config.modules.services.onepassword-systemd-credentials;
  backupUser = "databasus_invoiceninja";
  databaseName = "invoiceninja";
  deweyMgmtAddress = builtins.head config.site.net.mgmt.hosts4.${config.networking.hostName};
  maliMgmtAddress = builtins.head config.site.net.mgmt.hosts4.mali;
  # as invoiceninja2 user: podman unshare cat /proc/self/uid_map
  rootDir = "/var/lib/invoiceninja2";
  appEnvironmentFile = "/run/invoiceninja-env/invoiceninja.env";
  containerPort = "8000";
  inEnv = [
    "APP_ENV=production"
    "APP_URL=https://${cfg.domain}"
    "APP_DEBUG=false"
    "REQUIRE_HTTPS=false"
    "PHANTOMJS_PDF_GENERATION=false"
    "PDF_GENERATOR=snappdf"
    "TRUSTED_PROXIES=*"
    "CACHE_DRIVER=redis"
    "QUEUE_CONNECTION=redis"
    "SESSION_DRIVER=redis"
    "REDIS_HOST=redis"
    "REDIS_PORT=6379"
    "FILESYSTEM_DISK=debian_docker"
    "DB_PORT=3306"
    "DB_HOST=host.containers.internal"
    "DB_DATABASE=${databaseName}"
    "DB_USERNAME=${cfg.user.name}"
    "DB_CONNECTION=mysql"
    "IS_DOCKER=true"
    "SCOUT_DRIVER=null"
    "SERVER_NAME=:${containerPort}"
  ];
  inShared = {
    Network = "app.network";
    EnvironmentFile = [ appEnvironmentFile ];
    Environment = inEnv;
    Volume = [
      # ran as invoiceninja2 user: podman unshare chown -R 3015:3015 /var/lib/invoiceninja2/<VOL>
      "${rootDir}/cache:/var/www/html/bootstrap/cache:rw"
      "${rootDir}/storage:/app/storage:rw"
      "${rootDir}/caddy-config:/config:rw"
    ];
  };
in
{
  options.modules.services.invoiceninja = {
    enable = lib.mkEnableOption "invoiceninja";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "in.example.com";
      description = "The domain to use";
    };
    ports = {
      http = lib.mkOption {
        type = lib.types.port;
        description = "The HTTP port to use";
      };
    };
    subnet = lib.mkOption { type = lib.types.unspecified; };
    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption { type = lib.types.unspecified; };
  };

  config = lib.mkIf cfg.enable {

    site.gatus.endpoints = [
      {
        name = "Invoice Ninja";
        group = config.site.gatus.groups.work;
        url = "https://${cfg.domain}/";
      }
    ];

    modules.services.caddy.routes.clients = {
      publicHost = cfg.domain;
      upstream = "http://127.0.0.1:${toString cfg.ports.http}";
    };
    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/invoiceninja2"."mountpoint" = rootDir;
    };
    users.users.${cfg.user.name} = {
      inherit (cfg.user) name;
      inherit (cfg.user) uid;
      isNormalUser = true;
      linger = true;
      home = rootDir;
      createHome = false;
      inherit (cfg.user) group;
      # see https://github.com/nikstur/userborn/issues/7
      # autoSubUidGidRange = true;
    };
    users.groups.${cfg.group.name} = {
      inherit (cfg.group) name;
      gid = lib.mkForce cfg.group.gid;
    };
    assertions = [
      {
        assertion = onepassword.enable;
        message = "Invoice Ninja credentials require the 1Password systemd credential provider.";
      }
      {
        assertion = !(builtins.elem 3306 config.networking.firewall.allowedTCPPorts);
        message = "Invoice Ninja MariaDB must not be exposed through globally allowed TCP port 3306.";
      }
    ];

    modules.services.onepassword-systemd-credentials.consumers.invoiceninja-env-setup = {
      IN_USER_EMAIL = "op://home-ops-prod/invoiceninja/in-user-email";
      IN_PASSWORD = "op://home-ops-prod/invoiceninja/in-password";
      APP_KEY = "op://home-ops-prod/invoiceninja/app-key";
      DB_PASSWORD = "op://home-ops-prod/invoiceninja/db-password";
    };
    modules.services.onepassword-systemd-credentials.consumers.databasus-invoiceninja-role = {
      MARIADB_PASSWORD = "op://home-ops-prod/databasus-invoiceninja/password";
    };

    systemd.services.invoiceninja-env-setup = {
      description = "Prepare Invoice Ninja environment from 1Password credentials";
      wantedBy = [ "multi-user.target" ];
      wants = [ "user@${toString cfg.user.uid}.service" ];
      after = [ "user@${toString cfg.user.uid}.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        RuntimeDirectory = "invoiceninja-env";
        UMask = "0077";
      };
      script = ''
        : > ${appEnvironmentFile}
        printf '%s=%s\n' IN_USER_EMAIL "$(cat "$CREDENTIALS_DIRECTORY/IN_USER_EMAIL")" >> ${appEnvironmentFile}
        printf '%s=%s\n' IN_PASSWORD "$(cat "$CREDENTIALS_DIRECTORY/IN_PASSWORD")" >> ${appEnvironmentFile}
        printf '%s=%s\n' APP_KEY "$(cat "$CREDENTIALS_DIRECTORY/APP_KEY")" >> ${appEnvironmentFile}
        printf '%s=%s\n' DB_PASSWORD "$(cat "$CREDENTIALS_DIRECTORY/DB_PASSWORD")" >> ${appEnvironmentFile}
        chown ${cfg.user.name}:${cfg.group.name} ${appEnvironmentFile}
        ${config.systemd.package}/bin/systemctl \
          --machine=${cfg.user.name}@.host --user restart \
          invoiceninja-app.service invoiceninja-scheduler.service invoiceninja-worker.service
      '';
    };
    services.mysql = {
      ensureDatabases = [ databaseName ];
      # Rootless Podman's host.containers.internal forwarder needs an IPv4 wildcard listener;
      # nftables and the source-qualified MariaDB account provide the remote boundary.
      settings.mysqld.bind-address = "0.0.0.0";
      ensureUsers = [
        {
          inherit (cfg.user) name;
          ensurePermissions = {
            "${databaseName}.*" = "ALL PRIVILEGES";
          };
        }
      ];
    };

    systemd.services.databasus-invoiceninja-role = {
      description = "Provision the Databasus read-only role for Invoice Ninja";
      wantedBy = [ "multi-user.target" ];
      requires = [ "mysql.service" ];
      after = [ "mysql.service" ];
      script = ''
        password_b64="$(${pkgs.coreutils}/bin/base64 --wrap=0 "$CREDENTIALS_DIRECTORY/MARIADB_PASSWORD")"
        trap 'unset password_b64' EXIT

        ${config.services.mysql.package}/bin/mariadb --batch <<SQL
        SET @backup_password = FROM_BASE64('$password_b64');
        SET @statement = CONCAT(
          "CREATE USER IF NOT EXISTS '${backupUser}'@'${maliMgmtAddress}' IDENTIFIED BY ",
          QUOTE(@backup_password)
        );
        PREPARE create_user FROM @statement;
        EXECUTE create_user;
        DEALLOCATE PREPARE create_user;
        SET @statement = CONCAT(
          "ALTER USER '${backupUser}'@'${maliMgmtAddress}' IDENTIFIED BY ",
          QUOTE(@backup_password)
        );
        PREPARE alter_user FROM @statement;
        EXECUTE alter_user;
        DEALLOCATE PREPARE alter_user;
        REVOKE ALL PRIVILEGES, GRANT OPTION FROM '${backupUser}'@'${maliMgmtAddress}';
        GRANT SELECT, SHOW VIEW, LOCK TABLES, TRIGGER, EVENT
          ON ${databaseName}.* TO '${backupUser}'@'${maliMgmtAddress}';
        GRANT PROCESS, SHOW CREATE ROUTINE ON *.* TO '${backupUser}'@'${maliMgmtAddress}';
        SQL
      '';
      serviceConfig = {
        Type = "oneshot";
        User = "mysql";
        Group = "mysql";
        RemainAfterExit = true;
        ExecStartPre = "${pkgs.coreutils}/bin/test -S /run/mysqld/mysqld.sock";
      };
    };
    virtualisation.quadlet.enable = true;
    virtualisation.quadlet = {
      networks.app = {
        inherit (cfg.user) uid;
        autoStart = true;
        networkConfig = {
          Subnet = cfg.subnet.hostAddr;
        };
      };
      containers = {
        invoiceninja-redis = {
          inherit (cfg.user) uid;
          autoStart = true;
          serviceConfig = {
            RestartSec = "30";
            Restart = "always";
          };
          containerConfig = {
            # renovate: docker-image
            Image = "public.ecr.aws/docker/library/redis:8.8.0-alpine@sha256:9d317178eceac8454a2284a9e6df2466b93c745529947f0cd42a0fa9609d7005";
            Network = "app.network";
            Volume = [ "${rootDir}/redis:/data:rw" ];
            HealthCmd = "redis-cli ping";
            HealthInterval = "10s";
            HealthTimeout = "5s";
            HealthRetries = 5;
            Environment = [
              "TZ=Europe/Berlin"
            ];
            ContainerName = "redis";
          };
          unitConfig = {
            PartOf = [ "invoiceninja-app.service" ];
            BindsTo = [ "invoiceninja-app.service" ];
          };
        };
        invoiceninja-app = {
          inherit (cfg.user) uid;
          autoStart = false;
          containerConfig = {
            # renovate: docker-image
            Image = "ghcr.io/ramblurr/invoiceninja-octane:5.12.69@sha256:81a45bcd9b1040b96ddf7ab1cbe27c7d5936c0aec4269d364b9737016e84cbfb";
            Exec = "app --port=${containerPort} --workers=2 --log-level=info";
            PublishPort = [ "${toString cfg.ports.http}:${containerPort}" ];
            ContainerName = "app";
          }
          // inShared;
          unitConfig = {
            After = [ "invoiceninja-redis.service" ];
            Wants = [ "invoiceninja-redis.service" ];
          };
        };

        invoiceninja-scheduler = {
          inherit (cfg.user) uid;
          autoStart = false;
          serviceConfig = {
            RestartSec = "30";
            Restart = "always";
          };
          unitConfig = {
            After = [ "invoiceninja-app.service" ];
            Requires = [ "invoiceninja-app.service" ];
            PartOf = [ "invoiceninja-app.service" ];
          };
          containerConfig = {
            inherit (config.virtualisation.quadlet.containers.invoiceninja-app.containerConfig) Image;
            Exec = "scheduler --verbose";
            ContainerName = "scheduler";
          }
          // inShared;
        };
        invoiceninja-worker = {
          inherit (cfg.user) uid;
          autoStart = false;
          serviceConfig = {
            RestartSec = "30";
            Restart = "always";
          };
          unitConfig = {
            After = [ "invoiceninja-app.service" ];
            Requires = [ "invoiceninja-app.service" ];
            PartOf = [ "invoiceninja-app.service" ];
          };
          containerConfig = {
            inherit (config.virtualisation.quadlet.containers.invoiceninja-app.containerConfig) Image;
            Exec = "worker --verbose --sleep=3 --tries=3 --max-time=3600";
            ContainerName = "worker";
          }
          // inShared;
        };
      };
    };

    networking.firewall.extraInputRules = ''
      iifname "mgmt" ip saddr ${maliMgmtAddress}/32 ip daddr ${deweyMgmtAddress} tcp dport 3306 accept comment "Databasus Invoice Ninja logical backup"
    '';
  };
}
