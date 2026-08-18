{
  config,
  lib,
  ...
}:

let
  cfg = config.modules.services.invoiceninja;
  onepassword = config.modules.services.onepassword-systemd-credentials;
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
    "DB_DATABASE=invoiceninja"
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
    ];

    modules.services.onepassword-systemd-credentials.consumers.invoiceninja-env-setup = {
      IN_USER_EMAIL = "op://home-ops-prod/invoiceninja/in-user-email";
      IN_PASSWORD = "op://home-ops-prod/invoiceninja/in-password";
      APP_KEY = "op://home-ops-prod/invoiceninja/app-key";
      DB_PASSWORD = "op://home-ops-prod/invoiceninja/db-password";
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
      ensureDatabases = [ "invoiceninja" ];
      ensureUsers = [
        {
          inherit (cfg.user) name;
          ensurePermissions = {
            "invoiceninja.*" = "ALL PRIVILEGES";
          };
        }
      ];
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
  };
}
