{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.services.borgmatic;
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
        HEALTHCHECK_URL=https://
    */
    environment.systemPackages = with pkgs; [
      borgbackup
      borgmatic
      openssl
    ];
    sops.secrets.borgmatic-ssh-key = { };
    sops.secrets.borgmatic-env = { };
    systemd.services.borgmatic.serviceConfig.EnvironmentFile = "/run/secrets/borgmatic-env";
    systemd.timers.borgmatic.wantedBy = [ "timers.target" ];
    services.borgmatic = lib.mkIf cfg.enable {
      enable = true;
      enableConfigCheck = false; # We use environment variables in the config which aren't present during the config check
      settings = {
        inherit (cfg) repositories;
        source_directories = [ "/persist" ];
        exclude_caches = true;
        exclude_patterns = cfg.exclude-patterns;
        exclude_if_present = [ ".nobackup" ];
        #storage = {
        encryption_passphrase = "\${PASSPHRASE}";
        ssh_command = "ssh -o StrictHostKeyChecking=accept-new -o UserKnownHostsFile=/root/.ssh/known_hosts -o StrictHostKeyChecking=yes -i /run/secrets/borgmatic-ssh-key";
        archive_name_format = "${cfg.name}-{now:%Y-%m-%dT%H:%M:%S.%f}";
        #};
        #retention = {
        keep_within = "1d";
        keep_daily = 7;
        keep_weekly = 4;
        keep_monthly = 6;
        keep_yearly = 2;
        match_archives = "${cfg.name}";
        #};
        #consistency = {
        #prefix = "${cfg.name}";
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
        #};
        #hooks = {
        healthchecks = {
          ping_url = "\${HEALTHCHECK_URL}";
        };
        #};
      };
    };
  };
}
