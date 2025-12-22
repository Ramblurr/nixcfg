{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.services.koreader-sync;
  stateDir = "/var/lib/koreader-sync";
in
{
  options.modules.services.koreader-sync = {
    enable = lib.mkEnableOption "koreader-sync";
    domain = lib.mkOption {
      type = lib.types.str;
      description = "The domain to use for the calibre-web";
    };
    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
    };
    ports = {
      http = lib.mkOption { type = lib.types.port; };
    };
    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption { type = lib.types.unspecified; };
  };
  config = lib.mkIf cfg.enable {
    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/koreader-sync"."mountpoint" = stateDir;
      "rpool/encrypted/safe/svc/koreader-sync"."com.sun:auto-snapshot" = "false";
    };

    systemd.tmpfiles.rules = [
      "d '${stateDir}' 750 ${cfg.user.name} ${cfg.group.name} - -"
      "Z '${stateDir}' 750 ${cfg.user.name} ${cfg.group.name} - -"
      "d '${stateDir}/kosync' 750 ${cfg.user.name} ${cfg.group.name} - -"
      "Z '${stateDir}/kosync' 750 ${cfg.user.name} ${cfg.group.name} - -"
    ];
    users.users.${cfg.user.name} = {
      inherit (cfg.user) name;
      inherit (cfg.user) uid;
      isNormalUser = true;
      home = stateDir;
      createHome = false;
      group = lib.mkForce cfg.group.name;
      linger = true;
      # see https://github.com/nikstur/userborn/issues/7
      # autoSubUidGidRange = true;
    };
    users.groups.${cfg.group.name} = {
      inherit (cfg.group) name;
      gid = lib.mkForce cfg.group.gid;
    };
    virtualisation.oci-containers.containers.koreader-sync = {
      autoStart = false;
      # renovate: docker-image
      image = "ghcr.io/ramblurr/kosync:0.1.0";
      ports = [ "127.0.0.1:${toString cfg.ports.http}:3000" ];
      volumes = [ "${stateDir}/kosync:/srv/data:rw" ];
      podman = {
        user = cfg.user.name;
        sdnotify = "healthy";
      };
      extraOptions = [
        "--health-cmd=curl -s http://localhost:3000/healthcheck > /dev/null || exit 1"
        "--health-interval=30s"
        "--health-timeout=3s"
        "--health-start-period=5s"
        "--health-retries=3"
      ];
    };

    modules.services.ingress.virtualHosts.${cfg.domain} = {
      acmeHost = cfg.ingress.domain;
      upstream = "http://127.0.0.1:${toString cfg.ports.http}";
    };
  };
}
