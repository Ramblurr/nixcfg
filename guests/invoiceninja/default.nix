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
  inEnv = {
    APP_ENV = "production";
    APP_DEBUG = "false";
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

    DB_HOST = lib.mori.first config.site.net.svc.hosts4.dewey;
    DB_PORT = "3306";
    DB_DATABASE = "invoiceninja";
    DB_USERNAME = "invoiceninja";
    DB_CONNECTION = "mysql";

    IS_DOCKER = "true";
    SCOUT_DRIVER = "null";
  };
  inShared = {
    networks = [ "app.network" ];
    autoUpdate = "registry";
    userns = "keep-id:uid=999,gid=999";
    environmentFiles = [
      config.sops.secrets.app_env.path
    ];
    environments = inEnv;
    volumes = [
      "${rootDir}/cache:/var/www/html/bootstrap/cache:rw"
      "${rootDir}/storage:/app/storage:rw"
      "${rootDir}/storage-public:/app/public/:rw"
    ];
  };

  # ran on dewey
  # CREATE USER 'invoiceninja'@'172.20.20.21' IDENTIFIED BY 'password';
  # GRANT ALL ON invoiceninja.* TO 'invoiceninja'@'172.20.20.21';

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
        # sudo zfs create -V 5G tank/encrypted/svc/invoiceninja/var-tmp && sudo mkfs.ext4 /dev/zvol/tank/encrypted/svc/invoiceninja/var-tmp
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
    "d ${rootDir}/cache 0750 ${name} ${name}"
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

  sops.secrets.app_env = {
    owner = name;
    mode = "0400";
  };
  networking.firewall.allowedTCPPorts = [
    8080
  ];

  home-manager.users.${name} =
    { pkgs, config, ... }:
    {

      xdg.configFile."containers/containers.conf" = {
        text = ''
          [engine]
          env=["TMPDIR=${rootDir}/tmp"]
        '';
      };
      virtualisation.quadlet.autoEscape = true;
      virtualisation.quadlet.networks.app = { };
      virtualisation.quadlet.containers = {
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
              "invoiceninja-redis.service"
            ];
            Requires = [
              "app-network.service"
              "invoiceninja-redis.service"
            ];
          };
          containerConfig = {
            # renovate: docker-image
            image = "ghcr.io/ramblurr/invoiceninja-octane:5.11.78";
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
            image = "ghcr.io/ramblurr/invoiceninja-octane:5.11.78";
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
            image = "ghcr.io/ramblurr/invoiceninja-octane:5.11.78";
            exec = "worker --verbose --sleep=3 --tries=3 --max-time=3600";
          } // inShared;
        };
      };
    };
}
