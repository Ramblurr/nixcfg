{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.services.invoiceninja;
  inherit (config.repo.secrets.home-ops.users.invoiceninja2) uid name;
  # as invoiceninja2 user: podman unshare cat /proc/self/uid_map
  subordinateUIDRangeStart = 362144;
  containerUid = subordinateUIDRangeStart + (uid - 1);
  rootDir = "/var/lib/invoiceninja2";
  containerPort = "8000";
  inEnv = [
    "APP_ENV=production"
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
    EnvironmentFile = [
      config.sops.secrets."invoiceninja/app_env".path
    ];
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
    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
    };
    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption { type = lib.types.unspecified; };
  };

  config = lib.mkIf cfg.enable {
    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [ cfg.domain ];
      };
    };

    modules.services.ingress.virtualHosts."${cfg.domain}" = {
      acmeHost = cfg.ingress.domain;
      upstream = "http://127.0.0.1:${toString cfg.ports.http}";
      extraConfig = ''
        client_max_body_size 0;
      '';
    };
    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/invoiceninja2"."mountpoint" = rootDir;
    };
    users.users.${cfg.user.name} = {
      name = cfg.user.name;
      uid = cfg.user.uid;
      isSystemUser = true;
      linger = true;
      home = rootDir;
      createHome = false;
      autoSubUidGidRange = true;
      group = cfg.user.group;
    };
    users.groups.${cfg.group.name} = {
      name = cfg.group.name;
      gid = lib.mkForce cfg.group.gid;
    };
    sops.secrets."invoiceninja/app_env" = {
      sopsFile = ../../configs/home-ops/shared.sops.yml;
      owner = cfg.user.name;
      mode = "0400";
    };
    services.mysql = {
      ensureDatabases = [ "invoiceninja" ];
      ensureUsers = [
        {
          name = cfg.user.name;
          ensurePermissions = {
            "invoiceninja.*" = "ALL PRIVILEGES";
          };
        }
      ];
    };
    virtualisation.quadlet.enable = true;
    virtualisation.quadlet = {
      networks.app = {
        uid = cfg.user.uid;
        autoStart = true;
        networkConfig = {
          Subnet = cfg.subnet.hostAddr;
        };
      };
      containers = {
        invoiceninja-redis = {
          uid = cfg.user.uid;
          autoStart = true;
          serviceConfig = {
            RestartSec = "30";
            Restart = "always";
          };
          containerConfig = {
            Image = "public.ecr.aws/docker/library/redis:alpine";
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
          uid = cfg.user.uid;
          autoStart = true;
          containerConfig = {
            # renovate: docker-image
            Image = "ghcr.io/ramblurr/invoiceninja-octane:5.12.27";
            Exec = "app --port=${containerPort} --workers=2 --log-level=info";
            PublishPort = [ "${toString cfg.ports.http}:${containerPort}" ];
            ContainerName = "app";
          } // inShared;
          unitConfig = {
            After = [ "invoiceninja-redis.service" ];
            Wants = [ "invoiceninja-redis.service" ];
          };
        };

        invoiceninja-scheduler = {
          uid = cfg.user.uid;
          autoStart = true;
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
            Image = config.virtualisation.quadlet.containers.invoiceninja-app.containerConfig.Image;
            Exec = "scheduler --verbose";
            ContainerName = "scheduler";
          } // inShared;
        };
        invoiceninja-worker = {
          uid = cfg.user.uid;
          autoStart = true;
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
            Image = config.virtualisation.quadlet.containers.invoiceninja-app.containerConfig.Image;
            Exec = "worker --verbose --sleep=3 --tries=3 --max-time=3600";
            ContainerName = "worker";
          } // inShared;
        };
      };
    };
  };
}
