{
  config,
  lib,
  utils,
  ...
}:
let
  cfg = config.modules.services.tubearchivist;
  mediaLocalPath = "/mnt/mali/${cfg.mediaNfsShare}";
  stateDir = "/var/lib/tubearchivist";
  serviceDeps = [ "${utils.escapeSystemdPath mediaLocalPath}.mount" ];
  networkName = "tubearchivist-br";
in
{
  options.modules.services.tubearchivist = {
    enable = lib.mkEnableOption "tubearchivist";
    domain = lib.mkOption {
      type = lib.types.str;
      description = "The domain to use for tubearchivist";
    };
    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
    };
    port = lib.mkOption {
      type = lib.types.port;
      description = "The external port for tubearchivist web UI";
    };
    mediaNfsShare = lib.mkOption {
      type = lib.types.str;
      description = "NFS share path for media (e.g., tank2/media/youtube)";
    };
    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption { type = lib.types.unspecified; };
  };

  config = lib.mkIf cfg.enable {
    users.users.${cfg.user.name} = {
      inherit (cfg.user) name;
      uid = lib.mkForce cfg.user.uid;
      isSystemUser = true;
      group = lib.mkForce cfg.group.name;
    };

    users.groups.${cfg.group.name} = {
      inherit (cfg.group) name;
      gid = lib.mkForce cfg.group.gid;
    };

    fileSystems."${mediaLocalPath}" = {
      device = "${lib.my.cidrToIp config.repo.secrets.global.nodes.mali.dataCIDR}:/mnt/${cfg.mediaNfsShare}";
      fsType = "nfs";
    };

    modules.zfs.datasets.properties = {
      "tank/encrypted/svc/tubearchivist"."mountpoint" = stateDir;
      "tank/encrypted/svc/tubearchivist"."com.sun:auto-snapshot" = "false";
    };

    systemd.tmpfiles.rules = [
      "d ${stateDir}/cache 0755 ${cfg.user.name} ${cfg.group.name} -"
      "d ${stateDir}/es 0755 1000 1000 -"
      "d ${stateDir}/redis 0755 999 999 -"
    ];

    systemd.services.init-tubearchivist-network = {
      description = "Create the network bridge for tubearchivist";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig.Type = "oneshot";
      script =
        let
          podman = "${config.virtualisation.podman.package}/bin/podman";
        in
        ''
          ${podman} network inspect ${networkName} || ${podman} network create ${networkName}
        '';
    };

    systemd.services.podman-tubearchivist.after = serviceDeps;
    systemd.services.podman-tubearchivist.bindsTo = serviceDeps;

    virtualisation.oci-containers.containers = {
      tubearchivist = {
        autoStart = true;
        image = "docker.io/bbilly1/tubearchivist:v0.5.8";
        ports = [ "127.0.0.1:${toString cfg.port}:8000" ];
        volumes = [
          "${mediaLocalPath}:/youtube"
          "${stateDir}/cache:/cache"
        ];
        environment = {
          #DJANGO_DEBUG = "true";
          ELASTIC_PASSWORD = "tubearchivist";
          ES_URL = "http://tubearchivist-es:9200";
          REDIS_CON = "redis://tubearchivist-redis:6379";
          TA_AUTH_PROXY_USERNAME_HEADER = "X_AUTHENTIK_USERNAME";
          TA_ENABLE_AUTH_PROXY = "true";
          TA_HOST = "https://${cfg.domain} http://127.0.0.1:${toString cfg.port}";
          TA_LOGIN_AUTH_MODE = "forwardauth";
          TA_PASSWORD = "admin";
          TA_USERNAME = "admin";
          TZ = "Europe/Berlin";
        };
        dependsOn = [
          "tubearchivist-es"
          "tubearchivist-redis"
        ];
        extraOptions = [ "--network=${networkName}" ];
      };

      tubearchivist-es = {
        autoStart = true;
        image = "docker.io/bbilly1/tubearchivist-es:8.18.2";
        environment = {
          ELASTIC_PASSWORD = "tubearchivist";
          ES_JAVA_OPTS = "-Xms1g -Xmx1g";
          "xpack.security.enabled" = "true";
          "discovery.type" = "single-node";
          "path.repo" = "/usr/share/elasticsearch/data/snapshot";
        };
        volumes = [ "${stateDir}/es:/usr/share/elasticsearch/data" ];
        extraOptions = [
          "--network=${networkName}"
          "--ulimit=memlock=-1:-1"
        ];
      };

      tubearchivist-redis = {
        autoStart = true;
        image = "docker.io/library/redis:8.4.0";
        volumes = [ "${stateDir}/redis:/data" ];
        extraOptions = [ "--network=${networkName}" ];
      };
    };

    modules.services.ingress.virtualHosts.${cfg.domain} = {
      acmeHost = cfg.ingress.domain;
      upstream = "http://127.0.0.1:${toString cfg.port}";
      forwardAuth = true;
      extraConfig = ''
        client_max_body_size 0;
      '';
    };
  };
}
