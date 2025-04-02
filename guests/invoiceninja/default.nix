{
  lib,
  inputs,
  config,
  pkgs,
  ...
}:
let
  inherit (config.repo.secrets.global) domain;
  inherit (config.repo.secrets.home-ops.users.invoiceninja2) uid name;
  homeDir = "/home/invoiceninja";
  rootDir = "/var/lib/invoiceninja2";
  mariadbEnvFile = config.sops.templates.mariadb_env.path;

  nginxConf = pkgs.writeText "nginx.conf" (builtins.readFile ./nginx-laravel.conf);
  inEnv = {
    APP_ENV = "production";
    APP_DEBUG = "true";
    REQUIRE_HTTPS = "false";
    PHANTOMJS_PDF_GENERATION = "false";
    PDF_GENERATOR = "snappdf";
    TRUSTED_PROXIES = "*";

    CACHE_DRIVER = "redis";
    QUEUE_CONNECTION = "redis";
    SESSION_DRIVER = "redis";

    REDIS_HOST = "invoiceninja-redis";
    REDIS_PORT = "6379";

    FILESYSTEM_DISK = "debian_docker";

    DB_HOST = "invoiceninja-mariadb";
    DB_PORT = "3306";
    DB_DATABASE = "ninja";
    DB_USERNAME = "ninja";
    DB_CONNECTION = "mysql";

    IS_DOCKER = "true";
    SCOUT_DRIVER = "null";
  };
  inShared = {
    networks = [ "app.network" ];
    autoUpdate = "registry";
    userns = "keep-id:uid=999,gid=999";
    environmentFiles = [
      config.sops.templates.app_db_env.path
      config.sops.secrets.app_env.path
    ];
    environments = inEnv;
    volumes = [
      "${rootDir}/cache:/var/www/html/bootstrap/cache:rw"
      "${rootDir}/storage:/app/storage:rw"
    ];
  };

in
{
  system.stateVersion = "24.11";
  repo.secretFiles.home-ops = ../../secrets/home-ops.nix;
  sops.defaultSopsFile = ./secrets.sops.yaml;

  microvm = {
    hypervisor = "qemu"; # required atm for sops bootstrapping
    mem = 4096;
    volumes = [
      {

        # podman needs /var/tmp with lots of space for downloading images
        # provisioned manually on host with:
        # sudo zfs create tank/encrypted/svc/invoiceninja -o mountpoint=none
        # sudo zfs create -V 10G tank/encrypted/svc/invoiceninja-podman && sudo mkfs.ext4 /dev/zvol/tank/encrypted/svc/invoiceninja-podman
        mountPoint = "/var/tmp";
        image = "/dev/zvol/tank/encrypted/svc/invoiceninja/var-tmp";
        autoCreate = false;
        fsType = "ext4";
      }
      {
        # provisioned manually on host with:
        # sudo zfs create -V 10G tank/encrypted/svc/invoiceninja/var-lib-podman && sudo mkfs.ext4 /dev/zvol/tank/encrypted/svc/invoiceninja/var-lib-podman
        mountPoint = "/var/lib/podman/invoiceninja2";
        image = "/dev/zvol/tank/encrypted/svc/invoiceninja/var-lib-podman";
        autoCreate = false;
        fsType = "ext4";
      }
    ];
  };
  modules.microvm-guest = {
    host = "dewey";
    hostFQDN = "dewey.prim.${domain.home}";
    homeManager = {
      enable = true;
      username = config.repo.secrets.home-ops.users.invoiceninja2.name;
      uid = config.repo.secrets.home-ops.users.invoiceninja2.uid;
      gid = config.repo.secrets.home-ops.groups.invoiceninja2.gid;
    };
    quadlet.enable = true;
  };

  systemd.tmpfiles.rules = [
    "d ${rootDir} 0750 ${name} ${name}"
    "d ${rootDir}/public 0750 ${name} ${name}"
    "d ${rootDir}/cache 0750 ${name} ${name}"
    "d ${rootDir}/mysql 0750 ${name} ${name}"
    "d ${rootDir}/redis 0750 ${name} ${name}"
    "d ${rootDir}/storage 0750 ${name} ${name}"
    "d ${rootDir}/storage/framework 0750 ${name} ${name}"
    "d ${rootDir}/storage/framework/sessions 0750 ${name} ${name}"
    "d ${rootDir}/storage/framework/views 0750 ${name} ${name}"
    "d ${rootDir}/storage/framework/cache 0750 ${name} ${name}"
    "d ${rootDir}/storage/framework/cache/data 0750 ${name} ${name}"
    "Z /var/lib/podman/invoiceninja2 0750 ${name} ${name}"
  ];

  microvm.shares =
    let
      dir = rootDir;
      tag = builtins.replaceStrings [ "/" ] [ "_" ] dir;
    in
    [
      {
        inherit tag;
        source = rootDir;
        mountPoint = dir;
        proto = "virtiofs";
      }
    ];

  sops.secrets = {
    db_pass = { };
    db_root_pass = { };
    app_env = { };
  };

  sops.templates.mariadb_env = {
    owner = name;
    mode = "0400";
    content = ''
      MARIADB_PASSWORD=${config.sops.placeholder.db_pass}
      MARIADB_ROOT_PASSWORD=${config.sops.placeholder.db_root_pass}
    '';
  };

  sops.templates.app_db_env = {
    owner = name;
    mode = "0400";
    content = ''
      DB_PASSWORD=${config.sops.placeholder.db_pass}
      DB_ROOT_PASSWORD=${config.sops.placeholder.db_root_pass}
    '';
  };

  networking.firewall.allowedTCPPorts = [
    8080
  ];

  home-manager.users.${name} =
    { pkgs, config, ... }:
    {
      virtualisation.quadlet.autoEscape = true;

      virtualisation.quadlet.networks.app = { };
      virtualisation.quadlet.containers = {
        invoiceninja-mariadb = {
          autoStart = true;
          unitConfig = {
            After = [ "app-network.service" ];
            Requires = [ "app-network.service" ];
          };
          serviceConfig = {
            RestartSec = "10";
            Restart = "always";
          };
          containerConfig = {
            image = "docker.io/library/mariadb:11";
            networks = [ "app.network" ];
            autoUpdate = "registry";
            userns = "keep-id";
            environmentFiles = [ mariadbEnvFile ];
            environments = {
              MARIADB_DATABASE = "ninja";
              MARIADB_USER = "ninja";
            };
            volumes = [
              "${rootDir}/mysql:/var/lib/mysql:rw"
            ];
            healthCmd = "mariadb-admin -uroot -p$MARIADB_ROOT_PASSWORD ping";
            healthInterval = "10s";
            healthTimeout = "5s";
            healthRetries = 5;
          };
        };

        invoiceninja-redis = {
          autoStart = true;
          unitConfig = {
            After = [ "app-network.service" ];
            Requires = [ "app-network.service" ];
          };
          serviceConfig = {
            RestartSec = "10";
            Restart = "always";
          };
          containerConfig = {
            image = "docker.io/library/redis:alpine";
            networks = [ "app.network" ];
            autoUpdate = "registry";
            volumes = [
              "${rootDir}/redis:/data:rw"
            ];
            healthCmd = "redis-cli ping";
            healthInterval = "10s";
            healthTimeout = "5s";
            healthRetries = 5;
          };
        };

        invoiceninja-app = {
          autoStart = true;
          serviceConfig = {
            RestartSec = "10";
            Restart = "always";
          };
          unitConfig = {
            After = [
              "app-network.service"
              "invoiceninja-mariadb.service"
              "invoiceninja-redis.service"
            ];
            Requires = [
              "app-network.service"
              "invoiceninja-mariadb.service"
              "invoiceninja-redis.service"
            ];
          };
          containerConfig = {
            # renovate: docker-image
            image = "ghcr.io/ramblurr/invoiceninja-octane:5.11.62";
            exec = "app --port=8080 --workers=2 --log-level=info";
            publishPorts = [ "8080:8080" ];
          } // inShared;
        };

        invoiceninja-scheduler = {
          autoStart = true;
          serviceConfig = {
            RestartSec = "10";
            Restart = "always";
          };
          unitConfig = {
            After = [ "invoiceninja-app.service" ];
            Requires = [ "invoiceninja-app.service" ];
          };
          containerConfig = {
            # renovate: docker-image
            image = "ghcr.io/ramblurr/invoiceninja-octane:5.11.62";
            exec = "scheduler --verbose";
          } // inShared;
        };
        invoiceninja-worker = {
          autoStart = true;
          serviceConfig = {
            RestartSec = "10";
            Restart = "always";
          };
          unitConfig = {
            After = [ "invoiceninja-app.service" ];
            Requires = [ "invoiceninja-app.service" ];
          };
          containerConfig = {
            # renovate: docker-image
            image = "ghcr.io/ramblurr/invoiceninja-octane:5.11.62";
            exec = "worker --verbose --sleep=3 --tries=3 --max-time=3600";
          } // inShared;
        };
      };
    };
}
