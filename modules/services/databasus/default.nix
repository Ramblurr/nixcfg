{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.services.databasus;
  upstream = "http://127.0.0.1:${toString cfg.port}";
in
{
  options.modules.services.databasus = {
    enable = lib.mkEnableOption "Databasus database backups";

    image = lib.mkOption {
      type = lib.types.nonEmptyStr;
      default = "docker.io/databasus/databasus:v3.53.0@sha256:ed017f58674a18173a924cc7fd8059121adbd0aa6171d68357c4398fa14e0763";
      description = "Databasus container image.";
    };

    domain = lib.mkOption {
      type = lib.types.nonEmptyStr;
      example = "databasus.example.com";
      description = "Domain used to access Databasus.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 4005;
      description = "Loopback port for the Databasus web interface.";
    };

    dataDir = lib.mkOption {
      type = lib.types.path;
      default = "/var/lib/databasus";
      description = "Directory containing Databasus state, its encryption key, and local backups.";
    };

    dataset = lib.mkOption {
      type = lib.types.nonEmptyStr;
      default = "${config.modules.boot.zfs.rootPool}/encrypted/safe/svc/databasus";
      description = "ZFS dataset containing Databasus state.";
    };

    allowedRemoteIPs = lib.mkOption {
      type = lib.types.listOf lib.types.nonEmptyStr;
      default = [ ];
      description = "Peer addresses and CIDRs allowed through the Caddy route.";
    };
  };

  config = lib.mkIf cfg.enable {
    modules.services.podman.enable = true;

    modules.zfs.datasets = {
      enable = true;
      properties.${cfg.dataset} = {
        atime = "off";
        compression = "zstd";
        mountpoint = cfg.dataDir;
      };
    };

    systemd.tmpfiles.rules = [ "d ${cfg.dataDir} 0750 root root -" ];

    systemd.services.podman-databasus = {
      requires = [ "zfs-datasets.service" ];
      after = [ "zfs-datasets.service" ];
      unitConfig.RequiresMountsFor = [ cfg.dataDir ];
    };

    virtualisation.oci-containers.containers.databasus = {
      autoStart = true;
      # renovate: docker-image
      inherit (cfg) image;
      ports = [ "127.0.0.1:${toString cfg.port}:4005" ];
      volumes = [ "${cfg.dataDir}:/databasus-data:rw" ];
      environment = {
        DATABASUS_URL = "https://${cfg.domain}";
        IS_DISABLE_ANONYMOUS_TELEMETRY = "true";
      };
      extraOptions = [
        "--health-cmd=databasus healthcheck"
        "--health-interval=30s"
        "--health-timeout=5s"
        "--health-start-period=60s"
        "--health-retries=3"
      ];
    };

    modules.services.caddy.routes.databasus = {
      publicHost = cfg.domain;
      inherit upstream;
      inherit (cfg) allowedRemoteIPs;
    };
  };
}
