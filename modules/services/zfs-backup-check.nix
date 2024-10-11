{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.modules.services.zfs-backup-check;
  checkScript = pkgs.writeScriptBin "check-zrepl-snapshot" ''
    #!${pkgs.bash}/bin/bash
    # This script is from  Callum Gare @callumgare
    # source: https://github.com/zrepl/zrepl/issues/394#issuecomment-1099778015
    set -e
    dataset="$1"
    timeToCheckForSnapshotsAfterHumanReadable="$2"
    endpointToPingOnSuccess="$3"
    endpointToPingOnFailure="$4"


    timeToCheckForSnapshotsAfter=$(date --date "$timeToCheckForSnapshotsAfterHumanReadable" +'%s')
    timeOfLastSnapshot=$(${pkgs.zfs}/bin/zfs list -Hp -t snapshot -r -o creation -s creation "$dataset" | tail -1)

    if [ $timeOfLastSnapshot -gt $timeToCheckForSnapshotsAfter ]; then
      echo "OK: $dataset"
      ${pkgs.curl}/bin/curl -fsS -m 10 --retry 5 -o /dev/null "$endpointToPingOnSuccess"
    else
      echo "FAIL: $dataset"
      ${pkgs.curl}/bin/curl -fsS -m 10 --retry 5 -o /dev/null "$endpointToPingOnFailure"
    fi
  '';
in
{

  options.modules.services.zfs-backup-check = {
    enable = lib.mkEnableOption "zfs backup check";
    calendar = lib.mkOption {
      type = lib.types.str;
      default = "*-*-* *:11:00";
      description = lib.mdDoc ''
        The calendar expression for the healthcheck timer.
      '';
      example = lib.literalExpression "*-*-* *:00:00";
    };
    healthchecks = lib.mkOption {
      type = lib.types.listOf (lib.types.attrsOf lib.types.str);
      default = [ ];
      description = lib.mdDoc ''
        List of healthchecks, each represented by an attribute set with 'hc-url' and 'dataset' keys.
      '';
      example = lib.literalExpression ''
        [
          {
            hc-url = "https://hc.example.com/ping/abc123";
            dataset = "tank/bar";
            time = "1 hour ago";
          }
          {
            hc-url = "https://hc.example.com/ping/def456";
            dataset = "tank/foo";
            time = "1 hour ago";
          }
        ]
      '';
    };
  };
  config = lib.mkIf cfg.enable {

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
      path = [
        checkScript
        pkgs.coreutils
      ];
      script = ''
        ${lib.concatMapStrings (hc: ''
          echo "Running healthcheck for dataset ${hc.dataset}"
          check-zrepl-snapshot ${hc.dataset} ${lib.escapeShellArg hc.time} ${lib.escapeShellArg hc.hc-url}  ${lib.escapeShellArg hc.hc-url}/fail
        '') cfg.healthchecks}
      '';
    };
  };
}
