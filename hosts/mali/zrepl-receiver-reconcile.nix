{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.rsyncnet-zrepl-reconcile;
  serviceName = "rsyncnet-zrepl-reconcile";
  runtimeUser = "rsyncnet-zrepl";
  stateDataset = "rpool2/encrypted/safe/svc/zrepl-reconcile";
  stateDirActual = "/var/lib/private/${serviceName}";
  expectedBundleId = "v1-c337a0f46626b904-initial";
  onepassword = config.modules.services.onepassword-systemd-credentials;
  expectedDatasets = pkgs.writeText "rsyncnet-zrepl-validation-datasets" (
    builtins.readFile ../../scripts/rsyncnet-zrepl-bootstrap/validation-datasets
  );
  reconciler = pkgs.writeShellApplication {
    name = serviceName;
    runtimeInputs = [
      pkgs.coreutils
      pkgs.gawk
      pkgs.gnugrep
      pkgs.openssh
    ];
    text = builtins.readFile ../../scripts/rsyncnet-zrepl-reconcile/reconcile.sh;
  };
in
{
  options.services.rsyncnet-zrepl-reconcile = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable the Mali-originated rsync.net zrepl receiver reconciliation service.";
    };

    timer.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Schedule automatic receiver reconciliation every 15 minutes.";
    };

    receiverHost = lib.mkOption {
      type = lib.types.strMatching "[A-Za-z0-9]([A-Za-z0-9-]*[A-Za-z0-9])?(\\.[A-Za-z0-9]([A-Za-z0-9-]*[A-Za-z0-9])?)*";
      description = "Receiver hostname from account-specific private wiring.";
    };

    receiverAlias = lib.mkOption {
      type = lib.types.strMatching "[A-Za-z0-9._-]+";
      default = "rsyncnet";
      description = "Non-secret bounded receiver label used in operational evidence.";
    };

    identityReference = lib.mkOption {
      type = lib.types.strMatching "^op://.+";
      description = "1Password reference for Mali's dedicated SSH private key.";
    };

    knownHostsReference = lib.mkOption {
      type = lib.types.strMatching "^op://.+";
      description = "1Password reference for the authenticated receiver known_hosts line.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = onepassword.enable;
        message = "rsyncnet-zrepl-reconcile credentials require the 1Password systemd credential provider";
      }
    ];

    modules.services.onepassword-systemd-credentials.consumers.${serviceName} = {
      identity = cfg.identityReference;
      known-hosts = cfg.knownHostsReference;
    };

    site.gatus.heartbeats.rsyncnet-zrepl-reconcile = lib.mkIf cfg.timer.enable {
      service = serviceName;
      name = "rsync.net Zrepl Receiver Reconciliation";
      group = config.site.gatus.groups.infrastructure;
      interval = "45m";
      reporterAsRoot = true;
    };

    modules.zfs.datasets.properties.${stateDataset} = {
      atime = "off";
      compression = "zstd";
      mountpoint = stateDirActual;
    };

    systemd.services.${serviceName} = {
      description = "Reconcile the persistent rsync.net zrepl receiver bundle";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      unitConfig.RequiresMountsFor = [ stateDirActual ];
      environment = {
        EXPECTED_BUNDLE_ID = expectedBundleId;
        EXPECTED_DATASETS_FILE = expectedDatasets;
        RECEIVER_ALIAS = cfg.receiverAlias;
        RECEIVER_HOST = cfg.receiverHost;
        SSH_DEADLINE_SECONDS = "900";
      };
      serviceConfig = {
        Type = "oneshot";
        DynamicUser = true;
        User = runtimeUser;
        ExecStart = lib.getExe reconciler;
        TimeoutStartSec = "20min";
        StateDirectory = serviceName;
        StateDirectoryMode = "0700";
        RuntimeDirectory = serviceName;
        RuntimeDirectoryMode = "0700";
        UMask = "0077";

        CapabilityBoundingSet = "";
        LockPersonality = true;
        MemoryDenyWriteExecute = true;
        NoNewPrivileges = true;
        PrivateDevices = true;
        PrivateTmp = true;
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHome = true;
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectProc = "invisible";
        ProtectSystem = "strict";
        RemoveIPC = true;
        RestrictAddressFamilies = [
          "AF_UNIX"
          "AF_INET"
          "AF_INET6"
        ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        SystemCallArchitectures = "native";
      };
    };

    systemd.timers.${serviceName} = {
      description = "Run rsync.net zrepl receiver reconciliation every 15 minutes";
      wantedBy = lib.optionals cfg.timer.enable [ "timers.target" ];
      timerConfig = {
        OnCalendar = "*:0/15";
        RandomizedDelaySec = "2min";
        Persistent = true;
        Unit = "${serviceName}.service";
      };
    };
  };
}
