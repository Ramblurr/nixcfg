{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.services.borgmatic;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.services.borgmatic = {
    enable = mkBoolOpt false;
    name = mkOption {
      type = types.str;
    };

    exclude-patterns = mkOption {
      type = types.listOf types.str;
      default = [];
    };
    repositories = mkOption {
      type = types.listOf types.str;
      description = "Paths to repositories.";
      example =
        literalExpression ''["ssh://myuser@myrepo.myserver.com/./repo"]'';
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
    environment.systemPackages = with pkgs; [borgbackup borgmatic openssl];
    sops.secrets.borgmatic-ssh-key = {};
    sops.secrets.borgmatic-env = {};
    systemd.services.borgmatic.serviceConfig.EnvironmentFile = "/run/secrets/borgmatic-env";
    systemd.timers.borgmatic.wantedBy = ["timers.target"];
    services.borgmatic = lib.mkIf cfg.enable {
      enable = true;
      settings.location = {
        repositories = cfg.repositories;
        source_directories = ["/persist"];
        exclude_caches = true;
        exclude_patterns = cfg.exclude-patterns;
        exclude_if_present = [".nobackup"];
      };
      settings.storage = {
        encryption_passphrase = "\${PASSPHRASE}";
        ssh_command = "ssh -o StrictHostKeyChecking=accept-new -o UserKnownHostsFile=/root/.ssh/known_hosts -o StrictHostKeyChecking=yes -i /run/secrets/borgmatic-ssh-key";
        archive_name_format = "${cfg.name}-{now:%Y-%m-%dT%H:%M:%S.%f}";
      };
      settings.retention = {
        keep_within = "1d";
        keep_daily = 7;
        keep_weekly = 4;
        keep_monthly = 6;
        keep_yearly = 2;
        prefix = "${cfg.name}";
      };
      settings.consistency = {
        prefix = "${cfg.name}";
      };
      settings.hooks = {
        healthchecks = "\${HEALTHCHECK_URL}";
      };
    };
  };
}
