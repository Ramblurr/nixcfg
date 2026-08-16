{
  config,
  lib,
  pkgs,
  ...
}:
let
  homeDomain = config.repo.secrets.global.domain.home;
  managementAddress = builtins.head config.site.net.mgmt.hosts4.mali;
  managementNetwork = config.site.net.mgmt.subnet4;
  dataNetwork = config.site.net.data.subnet4;
  metadataDir = "/mnt/fast/services/garage/metadata";
  dataDir = "/mnt/tank2/services/garage/data";
  metadataSnapshotsDir = "/mnt/tank2/services/garage/metadata-snapshots";
  garagePorts = {
    s3 = 3900;
    rpc = 3901;
    admin = 3903;
  };
in
{
  users.groups.garage = { };
  users.users.garage = {
    isSystemUser = true;
    group = "garage";
  };

  modules.zfs.datasets = {
    enable = true;
    properties = {
      "fast/services/garage" = {
        canmount = "off";
        mountpoint = "none";
      };
      "fast/services/garage/metadata" = {
        atime = "off";
        compression = "zstd";
        mountpoint = metadataDir;
      };
      "tank2/services/garage" = {
        atime = "off";
        compression = "zstd";
        mountpoint = "/mnt/tank2/services/garage";
      };
      "tank2/services/garage/data" = {
        atime = "off";
        compression = "zstd";
        mountpoint = dataDir;
      };
      "tank2/services/garage/metadata-snapshots" = {
        atime = "off";
        compression = "zstd";
        mountpoint = metadataSnapshotsDir;
      };
    };
  };

  sops.secrets = {
    "garage/rpc-secret" = {
      owner = "garage";
      group = "garage";
      mode = "0400";
    };
    "garage/admin-token" = {
      owner = "garage";
      group = "garage";
      mode = "0400";
    };
    "garage/metrics-token" = {
      owner = "garage";
      group = "garage";
      mode = "0400";
    };
  };

  systemd.tmpfiles.rules = [
    "d ${metadataDir} 0700 garage garage -"
    "d ${dataDir} 0700 garage garage -"
    "d ${metadataSnapshotsDir} 0700 garage garage -"
  ];

  services.garage = {
    enable = true;
    package = pkgs.garage_2;
    settings = {
      metadata_dir = metadataDir;
      data_dir = dataDir;
      metadata_snapshots_dir = metadataSnapshotsDir;
      metadata_auto_snapshot_interval = "6h";
      metadata_fsync = true;
      db_engine = "sqlite";
      replication_factor = 1;
      consistency_mode = "consistent";
      rpc_bind_addr = "${managementAddress}:${toString garagePorts.rpc}";
      rpc_public_addr = "${managementAddress}:${toString garagePorts.rpc}";
      rpc_secret_file = config.sops.secrets."garage/rpc-secret".path;
      s3_api = {
        api_bind_addr = "127.0.0.1:${toString garagePorts.s3}";
        s3_region = "us-east-1";
      };
      admin = {
        api_bind_addr = "${managementAddress}:${toString garagePorts.admin}";
        admin_token_file = config.sops.secrets."garage/admin-token".path;
        metrics_token_file = config.sops.secrets."garage/metrics-token".path;
        metrics_require_token = true;
      };
    };
  };

  systemd.services.garage = {
    requires = [
      "sops-install-secrets.service"
      "zfs-datasets.service"
    ];
    after = [
      "sops-install-secrets.service"
      "zfs-datasets.service"
    ];
    unitConfig = {
      AssertPathIsMountPoint = [
        metadataDir
        dataDir
        metadataSnapshotsDir
      ];
      RequiresMountsFor = [
        metadataDir
        dataDir
        metadataSnapshotsDir
      ];
    };
    serviceConfig = {
      DynamicUser = lib.mkForce false;
      User = "garage";
      Group = "garage";
      ReadWritePaths = lib.mkAfter [ metadataSnapshotsDir ];
    };
  };

  networking.firewall.interfaces.mgmt.allowedTCPPorts = [
    garagePorts.rpc
    garagePorts.admin
  ];

  modules.services.caddy.routes = {
    garage-data = {
      publicHost = "garage.data.${homeDomain}";
      upstream = "http://127.0.0.1:${toString garagePorts.s3}";
      allowedRemoteIPs = [ dataNetwork ];
      dialTimeout = "300s";
      flushInterval = "-1";
    };
    garage-management = {
      publicHost = "garage.mgmt.${homeDomain}";
      upstream = "http://127.0.0.1:${toString garagePorts.s3}";
      allowedRemoteIPs = [ managementNetwork ];
      dialTimeout = "300s";
      flushInterval = "-1";
    };
  };
}
