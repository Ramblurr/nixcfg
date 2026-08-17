{
  inputs,
  pkgs,
}:
let
  lib = inputs.nixpkgs.lib;
  backupServiceNames =
    services: lib.filter (lib.hasPrefix "pgbackrest-") (builtins.attrNames services);
  backupTimerNames = timers: lib.filter (lib.hasPrefix "pgbackrest-") (builtins.attrNames timers);

  mkEvaluated =
    extraConfig:
    lib.nixosSystem {
      system = pkgs.stdenv.hostPlatform.system;
      modules = [
        inputs.impermanence.nixosModules.impermanence
        ../modules/services/postgresql.nix
        ../modules/zfs-attrs.nix
        (
          { lib, ... }:
          {
            options.modules.impermanence.enable = lib.mkOption {
              type = lib.types.bool;
              default = false;
            };
            config = {
              nixpkgs.pkgs = pkgs;
              system.stateVersion = "25.11";
              modules.services.postgresql = {
                enable = true;
                package = pkgs.postgresql_15;
                repo1 = {
                  path = "/test/repo1";
                  bucket = "test-repo1";
                  endpoint = "https://s3.example.test";
                };
                repo2 = {
                  path = "/test/repo2";
                  bucket = "test-repo2";
                  endpoint = "https://s3.example.test";
                };
              };
            };
          }
        )
        extraConfig
      ];
    };

  disabled = mkEvaluated { };
  disabledObserved = {
    postgresqlEnabled = disabled.config.services.postgresql.enable;
    backupSecretProjected = builtins.hasAttr "pgbackrest/conf.d/secrets.conf" disabled.config.environment.etc;
    backupServices = backupServiceNames disabled.config.systemd.services;
    backupTimers = backupTimerNames disabled.config.systemd.timers;
    postgresqlDatasetPresent = builtins.hasAttr "rpool/encrypted/safe/svc/postgresql" disabled.config.modules.zfs.datasets.properties;
  };

  enabledMissingSecret = builtins.tryEval (
    (mkEvaluated { modules.services.postgresql.repo1.enable = true; })
    .config.system.build.toplevel.drvPath
  );

  enabled = mkEvaluated {
    modules.services.postgresql = {
      repo1.enable = true;
      secretsFile = "/run/secrets/pgbackrest-test";
    };
  };
  enabledObserved = {
    backupSecretSource = enabled.config.environment.etc."pgbackrest/conf.d/secrets.conf".source;
    backupServices = backupServiceNames enabled.config.systemd.services;
    backupTimers = backupTimerNames enabled.config.systemd.timers;
    initPresent = builtins.hasAttr "pgbackrest-init" enabled.config.systemd.services;
  };
in
assert
  disabledObserved == {
    postgresqlEnabled = true;
    backupSecretProjected = false;
    backupServices = [ ];
    backupTimers = [ ];
    postgresqlDatasetPresent = true;
  };
assert !enabledMissingSecret.success;
assert
  enabledObserved == {
    backupSecretSource = "/run/secrets/pgbackrest-test";
    backupServices = [
      "pgbackrest-diff-backup-repo1"
      "pgbackrest-full-backup-repo1"
      "pgbackrest-incr-backup-repo1"
      "pgbackrest-init"
    ];
    backupTimers = [
      "pgbackrest-diff-backup-repo1"
      "pgbackrest-full-backup-repo1"
      "pgbackrest-incr-backup-repo1"
    ];
    initPresent = true;
  };
pkgs.runCommand "postgresql-backup-secrets-test" { } ''
  touch "$out"
''
