{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.modules.services.zfs-backup-check;
  check = pkgs.callPackage ../../pkgs/zfs-snapshot-age.nix { };
in
{
  options.modules.services.zfs-backup-check = {
    enable = lib.mkEnableOption "ZFS backup snapshot-age checks";
    calendar = lib.mkOption {
      type = lib.types.str;
      default = "*-*-* *:11:00";
      description = "Calendar expression for the backup healthcheck timer";
    };
    heartbeatInterval = lib.mkOption {
      type = lib.types.str;
      default = "3h";
      description = "Maximum interval between successful checks, including timer jitter";
    };
    healthchecks = lib.mkOption {
      type = lib.types.listOf (
        lib.types.submodule {
          options = {
            dataset = lib.mkOption {
              type = lib.types.nonEmptyStr;
              description = "Dataset whose newest recursive snapshot is checked";
            };
            time = lib.mkOption {
              type = lib.types.nonEmptyStr;
              description = "Oldest acceptable snapshot time, in GNU date syntax";
              example = "1 hour ago";
            };
          };
        }
      );
      default = [ ];
      description = "Snapshot-age checks; every dataset must pass before reporting success";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.healthchecks != [ ];
        message = "ZFS backup health monitoring requires at least one dataset.";
      }
    ];
    site.gatus.heartbeats.zrepl-healthcheck = {
      service = "zrepl-healthcheck";
      name = "Zrepl Backup Health";
      group = config.site.gatus.groups.infrastructure;
      interval = cfg.heartbeatInterval;
    };
    systemd.timers.zrepl-healthcheck = {
      description = "ZFS backup healthcheck";
      timerConfig = {
        OnCalendar = cfg.calendar;
        RandomizedDelaySec = 300;
        Persistent = true;
      };
      wantedBy = [ "timers.target" ];
    };
    systemd.services.zrepl-healthcheck = {
      description = "ZFS backup healthcheck";
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      serviceConfig = {
        Type = "oneshot";
        User = "root";
      };
      script = ''
        result=0
        ${lib.concatMapStringsSep "\n" (hc: ''
          ${lib.getExe check} ${lib.escapeShellArg hc.dataset} ${lib.escapeShellArg hc.time} || result=1
        '') cfg.healthchecks}
        exit "$result"
      '';
    };
  };
}
