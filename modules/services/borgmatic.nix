{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.services.borgmatic;
  hostName = config.networking.hostName;
  gatusGroup = "Infrastructure & Operations";
  gatusEndpointName = "Borgmatic Backup (${hostName})";
  gatusEndpointKey = "infrastructure---operations_borgmatic-backup-(${lib.toLower hostName})";
  gatusUrl = "https://status.${config.repo.secrets.global.domain.home}";
  gatusStartFile = "/run/borgmatic/gatus-start";
  repository =
    with types;
    submodule {
      options = {
        path = mkOption {
          type = str;
          description = mdDoc ''
            Path to the repository
          '';
        };
        label = mkOption {
          type = str;
          description = mdDoc ''
            Label to the repository
          '';
        };
      };
    };
in
{
  options.modules.services.borgmatic = {
    enable = lib.mkEnableOption "";
    name = mkOption { type = types.str; };

    exclude-patterns = mkOption {
      type = types.listOf types.str;
      default = [ ];
    };
    repositories = mkOption {
      type = types.nullOr (types.listOf repository);
      description = "Paths to repositories.";
      example = [
        {
          path = "ssh://user@backupserver/./sourcehostname.borg";
          label = "backupserver";
        }
        {
          path = "/mnt/backup";
          label = "local";
        }
      ];
    };
  };
  config = mkIf cfg.enable {
    /*
      To use this module you must provide the secrets with the yaml:
      borgmatic-ssh-key: |
        ..the ssh private key for the repos here..
      borgmatic-env:
        PASSPHRASE=ssh key passphrase here
        BORGMATIC_GATUS_TOKEN=matching Gatus external endpoint token
    */
    environment.systemPackages = with pkgs; [
      borgbackup
      borgmatic
      openssl
    ];
    sops.secrets.borgmatic-ssh-key = { };
    sops.secrets.borgmatic-env = { };
    systemd.services.borgmatic.serviceConfig = {
      EnvironmentFile = "/run/secrets/borgmatic-env";
      RuntimeDirectory = "borgmatic";
    };
    systemd.timers.borgmatic = {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "*-*-* 04:00:00";
        Persistent = true;
        RandomizedDelaySec = "2h";
      };
    };

    site.gatus.externalEndpoints = [
      {
        name = gatusEndpointName;
        group = gatusGroup;
        token = "$BORGMATIC_GATUS_TOKEN";
        heartbeat.interval = "30h";
        alerts = [ { type = "pushover"; } ];
      }
    ];
    services.borgmatic = lib.mkIf cfg.enable {
      enable = true;
      enableConfigCheck = false; # We use environment variables in the config which aren't present during the config check
      settings = {
        inherit (cfg) repositories;
        source_directories = [ "/persist" ];
        exclude_caches = true;
        exclude_patterns = cfg.exclude-patterns;
        exclude_if_present = [ ".nobackup" ];
        encryption_passphrase = "\${PASSPHRASE}";
        ssh_command = "ssh -o StrictHostKeyChecking=accept-new -o UserKnownHostsFile=/root/.ssh/known_hosts -o StrictHostKeyChecking=yes -i /run/secrets/borgmatic-ssh-key";
        archive_name_format = "${cfg.name}-{now:%Y-%m-%dT%H:%M:%S.%f}";
        keep_within = "1d";
        keep_daily = 7;
        keep_weekly = 4;
        keep_monthly = 6;
        keep_yearly = 2;
        match_archives = "${cfg.name}";
        check_last = 3;
        checks = [
          {
            name = "repository";
            frequency = "4 weeks";
          }
          {
            name = "archives";
            frequency = "6 weeks";
          }
        ];
        commands = [
          {
            before = "action";
            when = [ "create" ];
            run = [ "${lib.getExe' pkgs.coreutils "date"} +%s > ${gatusStartFile}" ];
          }
          {
            after = "action";
            when = [ "create" ];
            states = [ "finish" ];
            run = [
              ''${lib.getExe pkgs.curl} --fail --silent --show-error --retry 3 --request POST --header "Authorization: Bearer $BORGMATIC_GATUS_TOKEN" "${gatusUrl}/api/v1/endpoints/${gatusEndpointKey}/external?success=true&duration=$(( $(${lib.getExe' pkgs.coreutils "date"} +%s) - $(cat ${gatusStartFile}) ))s" || echo "Failed to report Borgmatic success to Gatus" >&2''
            ];
          }
          {
            after = "error";
            when = [ "create" ];
            run = [
              ''${lib.getExe pkgs.curl} --fail --silent --show-error --retry 3 --get --request POST --header "Authorization: Bearer $BORGMATIC_GATUS_TOKEN" --data-urlencode "success=false" --data-urlencode error={error} "${gatusUrl}/api/v1/endpoints/${gatusEndpointKey}/external" || echo "Failed to report Borgmatic failure to Gatus" >&2''
            ];
          }
        ];
      };
    };
  };
}
