{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.homeassistant-backup-retention;
  serviceName = "homeassistant-backup-retention";
  directory = "/mnt/tank2/backups/homeassistant/hassos-new";
  prune = import ../../scripts/homeassistant-backup-retention { inherit pkgs; };
in
{
  options.services.homeassistant-backup-retention.enable = lib.mkEnableOption "daily pruning of Home Assistant backup tarballs";

  config = lib.mkIf cfg.enable {
    systemd.services.${serviceName} = {
      description = "Prune Home Assistant backup tarballs to a daily/weekly/monthly/yearly set";
      after = [ "zfs-mount.service" ];
      unitConfig = {
        RequiresMountsFor = [ directory ];
        AssertPathIsMountPoint = "/mnt/tank2/backups/homeassistant";
      };
      serviceConfig = {
        Type = "oneshot";
        User = "hassos";
        Group = "hassos";
        ExecStart = "${lib.getExe prune} ${directory}";
        ReadWritePaths = [ directory ];

        CapabilityBoundingSet = "";
        IPAddressDeny = "any";
        LockPersonality = true;
        MemoryDenyWriteExecute = true;
        NoNewPrivileges = true;
        PrivateDevices = true;
        PrivateNetwork = true;
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
        RestrictAddressFamilies = [ "AF_UNIX" ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        SystemCallArchitectures = "native";
        UMask = "0007";
      };
    };

    systemd.timers.${serviceName} = {
      description = "Prune Home Assistant backup tarballs daily";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "daily";
        RandomizedDelaySec = "1h";
        Persistent = true;
      };
    };
  };
}
