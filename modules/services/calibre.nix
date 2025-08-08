{
  options,
  config,
  lib,
  utils,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.services.calibre;
  home-ops = config.repo.secrets.home-ops;
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
    domain = lib.mkOption {
      type = lib.types.str;
      description = "The domain to use for the calibre";
    };
    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
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
      image = "lscr.io/linuxserver/calibre:8.8.0";
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

    modules.services.ingress.virtualHosts.${cfg.domain} = {
      acmeHost = cfg.ingress.domain;
      upstream = "http://127.0.0.1:${toString cfg.ports.gui}";
      forwardAuth = true;
      extraConfig = ''
        client_max_body_size 0;
      '';
    };
  };
}
