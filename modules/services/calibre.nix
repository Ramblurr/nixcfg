{
  config,
  lib,
  utils,
  ...
}:
let
  cfg = config.modules.services.calibre;
  mediaLocalPath = "/mnt/mali/${cfg.mediaNfsShare}";
  stateDir = "/var/lib/calibre";
  dlLocalPath = "/mnt/downloads";
  serviceDeps = [
    "${utils.escapeSystemdPath mediaLocalPath}.mount"
    "${utils.escapeSystemdPath dlLocalPath}.mount"
  ];
in
{
  options.modules.services.calibre = {
    enable = lib.mkEnableOption "calibre";
    domain = {
      gui = lib.mkOption {
        type = lib.types.str;
        description = "The domain to use for the calibre";
      };
      server = lib.mkOption {
        type = lib.types.str;
        description = "The domain to use for the calibre content server";
      };
    };
    ports = {
      gui = lib.mkOption { type = lib.types.port; };
      server = lib.mkOption { type = lib.types.port; };
    };
    mediaNfsShare = lib.mkOption { type = lib.types.str; };
    dlNfsShare = lib.mkOption { type = lib.types.str; };
    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption { type = lib.types.unspecified; };
  };
  config = lib.mkIf cfg.enable {
    #users.users.${cfg.user.name} = {
    #  name = cfg.user.name;
    #  uid = lib.mkForce cfg.user.uid;
    #  isSystemUser = true;
    #  group = lib.mkForce cfg.group.name;
    #};

    #users.groups.${cfg.group.name} = {
    #  name = cfg.group.name;
    #  gid = lib.mkForce cfg.group.gid;
    #};

    fileSystems."${mediaLocalPath}" = {
      device = "${lib.my.cidrToIp config.repo.secrets.global.nodes.mali.dataCIDR}:/mnt/${cfg.mediaNfsShare}";
      fsType = "nfs";
    };

    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/calibre"."mountpoint" = stateDir;
      "rpool/encrypted/safe/svc/calibre"."com.sun:auto-snapshot" = "false";
    };

    systemd.services.podman-calibre.after = serviceDeps;
    systemd.services.podman-calibre.bindsTo = serviceDeps;
    virtualisation.oci-containers.containers.calibre = {
      autoStart = true;
      # renovate: docker-image
      image = "lscr.io/linuxserver/calibre:9.13.0@sha256:f4a17653e7851c4642583ba2cd292509f90b4b8e123596af38b6ee02154676be";
      ports = [
        "127.0.0.1:${toString cfg.ports.gui}:8080"
        "127.0.0.1:${toString cfg.ports.server}:8081"
      ];
      volumes = [
        "${stateDir}:/config:rw"
        "${mediaLocalPath}/books:/media/books:rw"
        "${dlLocalPath}:/downloads:rw"
      ];
      environment = {
        TZ = "Europe/Berlin";
        PUID = "2000";
        PGID = "2000";
      };
      extraOptions = [ ];
    };

    modules.services.caddy.protectedRoutes.calibre-gui = {
      publicHost = cfg.domain.gui;
      upstream = "http://127.0.0.1:${toString cfg.ports.gui}";
    };
    modules.services.caddy.routes.calibre-server = {
      publicHost = cfg.domain.server;
      upstream = "http://127.0.0.1:${toString cfg.ports.server}";
      requestHeaders.Authorization = "{http.request.header.Authorization}";
    };

  };
}
