{
  pkgs,
  config,
  lib,
  ...
}:
#
# This module is highly specific to my needs, so be careful using it.
# It exists so that I can enable/disable services easily across my various servers.
# The idea is if I want to move service foo to host A, I can just enable it with one flag and it will be deployed
# (of course I'd have to migrate the data, but that's easy enough with zfs send recv)
let
  home-ops = config.repo.secrets.home-ops;
  cfg = config.home-ops;
in
{
  options.home-ops = {
    enable = lib.mkEnableOption "My modular multi-host Home Ops setup";
    postgresql = {
      enable = lib.mkEnableOption "Postgresql";
      onsiteBackup = {
        enable = lib.mkEnableOption "Onsite Backup";
        path = lib.mkOption {
          type = lib.types.str;
          default = "/test/${config.networking.hostName}/repo1";
        };
      };

      offsiteBackup = {
        enable = lib.mkEnableOption "Offsite Backup";
        path = lib.mkOption {
          type = lib.types.str;
          default = "/test/${config.networking.hostName}/repo2";
        };
      };
    };
    mariadb = {
      enable = lib.mkEnableOption "MariaDB";
    };
    ingress = {
      enable = lib.mkEnableOption "NGINX Ingress";
    };
    containers = {
      enable = lib.mkEnableOption "OCI containers";
    };

    apps = {
      echo-server.enable = lib.mkEnableOption "Echo Server";
      davis.enable = lib.mkEnableOption "Davis, carddav and caldav server";
      invoiceninja.enable = lib.mkEnableOption "Invoice Ninja";
      authentik.enable = lib.mkEnableOption "Authentik";
      paperless.enable = lib.mkEnableOption "Paperless";
      ocis-work.enable = lib.mkEnableOption "oCIS Work";
      ocis-home.enable = lib.mkEnableOption "oCIS Home";
      plex.enable = lib.mkEnableOption "Plex";
      tautulli.enable = lib.mkEnableOption "Tautulli";
      home-dl.enable = lib.mkEnableOption "Home *arr";
    };
  };
  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion =
          cfg.postgresql.enable -> cfg.postgresql.onsiteBackup.enable || cfg.postgresql.offsiteBackup.enable;
        message = "Postgresql must be configured with backup repositories";
      }
      {
        assertion = !(cfg.apps.ocis-work.enable && cfg.apps.ocis-home.enable);
        message = "OCIS Work and OCIS Home cannot be enabled at the same time on the same host";
      }
    ];

    environment.systemPackages = [
      pkgs.ripgrep
      pkgs.fd
      pkgs.ncdu
      pkgs.rclone
      pkgs.lshw
    ];

    #
    # Supporting services
    #
    home-ops.zrepl.enable = true;
    sops.secrets.pgbackrestSecrets = lib.mkIf cfg.postgresql.enable { mode = "400"; };
    modules.services.postgresql = lib.mkIf cfg.postgresql.enable {
      enable = true;
      package = pkgs.postgresql_15;
      secretsFile = config.sops.secrets.pgbackrestSecrets.path;
      repo1 = {
        enable = cfg.postgresql.onsiteBackup.enable;
        path = cfg.postgresql.onsiteBackup.path;
        bucket = home-ops.pgBackup.onsite.bucket;
        endpoint = home-ops.pgBackup.onsite.endpoint;
      };
      repo2 = {
        enable = cfg.postgresql.offsiteBackup.enable;
        path = cfg.postgresql.offsiteBackup.path;
        bucket = home-ops.pgBackup.offsite.bucket;
        endpoint = home-ops.pgBackup.offsite.endpoint;
      };
    };
    modules.services.mariadb = lib.mkIf cfg.mariadb.enable {
      enable = true;
      package = pkgs.mariadb_110;
    };
    modules.services.ingress = lib.mkIf cfg.ingress.enable {
      enable = true;
      domains = home-ops.domains;
    };

    virtualisation.podman.enable = cfg.containers.enable;
    virtualisation.oci-containers = lib.mkIf cfg.containers.enable { backend = "podman"; };

    users.groups.${home-ops.groups.media.name} = {
      gid = home-ops.groups.media.gid;
    };

    #
    # Application Services
    #
    modules.services.echo-server = lib.mkIf cfg.apps.echo-server.enable {
      enable = true;
      domain = "echo-test.${home-ops.homeDomain}";
      ports.http = home-ops.ports.echo-server;
      ingress = {
        external = true;
        domain = home-ops.homeDomain;
      };
    };

    modules.services.davis = lib.mkIf cfg.apps.davis.enable {
      enable = true;
      domain = "dav.${home-ops.homeDomain}";
      ingress = {
        external = true;
        domain = home-ops.homeDomain;
      };
    };

    modules.services.invoiceninja = lib.mkIf cfg.apps.invoiceninja.enable {
      enable = true;
      domain = "clients.${home-ops.workDomain}";
      ingress = {
        external = true;
        domain = home-ops.workDomain;
      };
    };

    modules.services.authentik = lib.mkIf cfg.apps.authentik.enable {
      enable = true;
      domain = "auth.${home-ops.homeDomain}";
      ports.http = home-ops.ports.authentik-http;
      ports.https = home-ops.ports.authentik-https;
      ingress = {
        external = true;
        domain = home-ops.homeDomain;
      };
    };

    modules.services.paperless = lib.mkIf cfg.apps.paperless.enable {
      enable = true;
      domain = "paperless.${home-ops.homeDomain}";
      ports.http = home-ops.ports.paperless-http;
      user = home-ops.users.paperless;
      group = home-ops.groups.paperless;
      nfsShare = "tank2/services/paperless";
      ingress = {
        domain = home-ops.homeDomain;
      };
    };

    modules.services.plex = lib.mkIf cfg.apps.plex.enable {
      enable = true;
      domain = "plex.${home-ops.homeDomain}";
      user = home-ops.users.plex;
      group = home-ops.groups.plex;
      nfsShare = "tank2/media";
      ingress = {
        domain = home-ops.homeDomain;
      };
    };

    modules.services.tautulli = lib.mkIf cfg.apps.tautulli.enable {
      enable = true;
      domain = "tautulli.${home-ops.homeDomain}";
      user = home-ops.users.tautulli;
      ports.http = home-ops.ports.tautulli-http;
      ingress = {
        domain = home-ops.homeDomain;
      };
    };

    modules.services.home-dl = lib.mkIf cfg.apps.home-dl.enable {
      enable = true;
      baseDomain = home-ops.homeDomain;
      ports = home-ops.ports.home-dl;
      mediaNfsShare = "tank2/media";
      dlNfsShare = "fast/downloads";
      ingress = {
        domain = home-ops.homeDomain;
      };
    };

    modules.services.ocis =
      if cfg.apps.ocis-work.enable then
        {
          enable = true;
          domain = "data.${home-ops.workDomain}";
          ports.http = home-ops.ports.ocis-http;
          user = home-ops.users.ocis-work;
          group = home-ops.groups.ocis-work;
          nfsShare = "tank2/services/work-ocis2";
          ingress = {
            domain = home-ops.workDomain;
          };
        }
      else if cfg.apps.ocis-home.enable then
        {
          enable = true;
          domain = "drive.${home-ops.homeDomain}";
          ports.http = home-ops.ports.ocis-http;
          user = home-ops.users.ocis-home;
          group = home-ops.groups.ocis-home;
          nfsShare = "tank2/services/home-ocis2";
          ingress = {
            domain = home-ops.homeDomain;
          };
        }
      else
        { };
  };
}
