{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.telemetry.prometheus;
  stateDir = "/var/lib/prometheus2";

  serviceDeps = [
    "var-lib-prometheus2.mount"
    "zfs-datasets.service"
  ];
in
{
  options.modules.telemetry.prometheus = {
    enable = lib.mkEnableOption "prometheus";
  };

  config = lib.mkIf cfg.enable {
    services.prometheus = {
      enable = true;

      # Thanos stores long term metrics
      retentionTime = "1d";

      extraFlags = [
        "--web.enable-admin-api"
        "--storage.tsdb.min-block-duration=2h"
        "--storage.tsdb.max-block-duration=2h"
      ];

      globalConfig.external_labels.prometheus = "${config.networking.hostName}";
    };

    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/prometheus"."mountpoint" = stateDir;
    };

    systemd.tmpfiles.rules = [
      "z '${stateDir}' 750 prometheus prometheus - -"
    ];

    networking.firewall.allowedTCPPorts = [
      config.services.prometheus.port
    ];

    services.borgmatic.configurations.system.exclude_patterns = [ "${stateDir}/data/wal" ];
    systemd.services.thanos-query = {
      requires = serviceDeps;
      after = serviceDeps;
    };
    systemd.services.thanos-store = {
      requires = serviceDeps;
      after = serviceDeps;
    };
    systemd.services.thanos-compact = {
      requires = serviceDeps;
      after = serviceDeps;
    };
  };

}
