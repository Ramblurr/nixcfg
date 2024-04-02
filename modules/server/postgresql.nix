{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  # PostgreSQL nixos module with mandatory S3 PITR backup with pgbackrest
  cfg = config.modules.server.postgresql;
  stanza = "db";

  withImpermanence = config.modules.impermanence.enable;

  fullBackupService = repo: {
    description = "pgBackRest Full Backup repo ${repo}";
    requires = [ "pgbackrest-init.service" ];
    after = [ "pgbackrest-init.service" ];
    serviceConfig = {
      Type = "oneshot";
      User = "postgres";
      Group = "postgres";
    };
    unitConfig = {
      ConditionPathExists = "${cfg.pgDataDir}/postgresql.conf";
    };
    script = "${pkgs.pgbackrest}/bin/pgbackrest --type=full --repo=${repo} --stanza=${stanza} backup";
  };

  diffBackupService = repo: {
    description = "pgBackRest Differential Backup repo ${repo}";
    requires = [
      "postgresql.service"
      "pgbackrest-init.service"
    ];
    after = [
      "postgresql.service"
      "pgbackrest-init.service"
    ];
    serviceConfig = {
      Type = "oneshot";
      User = "postgres";
      Group = "postgres";
    };
    unitConfig = {
      ConditionPathExists = "${cfg.pgDataDir}/postgresql.conf";
    };
    script = "${pkgs.pgbackrest}/bin/pgbackrest --type=diff --repo=${repo} --stanza=${stanza} backup";
  };

  incrBackupService = repo: {
    description = "pgBackRest Incremental Backup repo ${repo}";
    requires = [
      "postgresql.service"
      "pgbackrest-init.service"
    ];
    after = [
      "postgresql.service"
      "pgbackrest-init.service"
    ];
    serviceConfig = {
      Type = "oneshot";
      User = "postgres";
      Group = "postgres";
    };
    unitConfig = {
      ConditionPathExists = "${cfg.pgDataDir}/postgresql.conf";
    };
    script = "${pkgs.pgbackrest}/bin/pgbackrest --type=incr --repo=${repo} --stanza=${stanza} backup";
  };

  timerBase = onCal: name: {
    description = "pgBackRest ${name} Backup";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = onCal;
      Persistent = true; # Ensures the timer catches up if missed
    };
  };
in
{
  options.modules.server.postgresql = {
    enable = mkEnableOption "postgresql";

    package = mkPackageOption pkgs "postgresql_15" { };

    pgDataDir = mkOption {
      type = types.str;
      example = "/var/lib/postgresql/15";
      default = config.services.postgresql.dataDir;
      description = "The data directory for the PostgreSQL instance";
    };
    secretsFile = mkOption {
      type = types.path;
      example = "/run/secrets/pgbackrest.conf";
      description = "Path to a pgbackrest configuration file snippet, it should contain repo1-s3-key, repo1-cipher-pass, etc.";
    };
    repo1 = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable the offsite backup repository";
      };
      path = mkOption {
        type = types.str;
        example = "/app-db/repo1";
        description = "The path to the primary backup repository";
      };
      bucket = mkOption {
        type = types.str;
        example = "pg-backups";
        description = "The name of the S3 bucket to use for the primary backup repository";
      };
      endpoint = mkOption {
        type = types.str;
        example = "https://s3.example.com";
        description = "The endpoint of the S3 bucket to use for the primary backup repository";
      };
      region = mkOption {
        type = types.str;
        default = "us-east-1";
        description = "The region of the S3 bucket to use for the primary backup repository";
      };
      uriStyle = mkOption {
        type = types.str;
        default = "path";
        description = "The URI style of the S3 bucket to use for the primary backup repository";
      };

      retentionDiff = mkOption {
        type = types.int;
        default = 30;
        description = "The number of differential backups to retain";
      };
      retentionFull = mkOption {
        type = types.int;
        default = 14;
        description = "The number of full backups to retain";
      };
      timers = {
        full = mkOption {
          type = types.str;
          default = "Sun 01:00";
          description = "The time to run the full backup";
        };
        diff = mkOption {
          type = types.str;
          default = "Mon..Sat 01:00";
          description = "The time to run the differential backup";
        };
        incr = mkOption {
          type = types.str;
          # Every hour except 1-2am
          default = "*-*-* 03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23:00:00";
          description = "The time to run the incremental backup";
        };
      };
    };

    repo2 = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable the offsite backup repository";
      };
      path = mkOption {
        type = types.str;
        example = "/app-db/repo2";
        description = "The path to the offsite backup repository";
      };

      bucket = mkOption {
        type = types.str;
        example = "pg-backups";
        description = "The name of the S3 bucket to use for the repository";
      };
      endpoint = mkOption {
        type = types.str;
        example = "https://s3.example.com";
        description = "The endpoint of the S3 bucket to use for the repository";
      };
      region = mkOption {
        type = types.str;
        default = "us-east-1";
        description = "The region of the S3 bucket to use for the repository";
      };
      uriStyle = mkOption {
        type = types.str;
        default = "path";
        description = "The URI style of the S3 bucket to use for the repository";
      };
      retentionDiff = mkOption {
        type = types.int;
        default = 7;
        description = "The number of differential backups to retain";
      };
      retentionFull = mkOption {
        type = types.int;
        default = 2;
        description = "The number of full backups to retain";
      };
      timers = {
        full = mkOption {
          type = types.str;
          default = "Wed 13:00";
          description = "The time to run the full backup";
        };
        diff = mkOption {
          type = types.str;
          # Sun-Sat, excl Wed, at 13:00
          default = "Sun,Mon,Tue,Thu,Fri,Sat 13:00";
          description = "The time to run the differential backup";
        };
        incr = mkOption {
          type = types.str;
          # Every hour except 13:00-14:00
          default = "*-*-* 00,01,02,03,04,05,06,07,08,09,10,11,12,14,15,16,17,18,19,20,21,22,23:30:00";
          description = "The time to run the incremental backup";
        };
      };
    };
  };

  config = mkIf cfg.enable {
    environment.etc."pgbackrest/pgbackrest.conf" = {
      user = "postgres";
      group = "postgres";
      text = ''
        [main]
      '';
    };
    environment.etc."pgbackrest/conf.d/secrets.conf" = {
      user = "postgres";
      group = "postgres";
      mode = "0600";
      source = cfg.secretsFile;
    };
    environment.etc."pgbackrest/conf.d/instance.conf" = {
      user = "postgres";
      group = "postgres";
      text =
        ''
          [global]
          archive-async=y
          archive-push-queue-max = 4GiB
          archive-timeout = 60
          compress-level = 9
          compress-type = lz4
          delta = y
          log-path = /var/log/pgbackrest
          spool-path = /var/spool/pgbackrest
        ''
        + optionalString cfg.repo1.enable ''
          repo1-block = y
          repo1-bundle = y
          repo1-path = ${cfg.repo1.path}
          repo1-retention-diff = ${toString cfg.repo1.retentionDiff}
          repo1-retention-full = ${toString cfg.repo1.retentionFull}
          repo1-retention-full-type = time
          repo1-s3-bucket = ${cfg.repo1.bucket}
          repo1-s3-endpoint = ${cfg.repo1.endpoint}
          repo1-s3-region = ${cfg.repo1.region}
          repo1-s3-uri-style = ${cfg.repo1.uriStyle}
          repo1-type = s3
        ''
        + optionalString cfg.repo2.enable ''

          repo2-block = y
          repo2-bundle = y
          repo2-path = ${cfg.repo2.path}
          repo2-retention-diff = ${toString cfg.repo2.retentionDiff}
          repo2-retention-full = ${toString cfg.repo2.retentionFull}
          repo2-retention-full-type = time
          repo2-s3-bucket = ${cfg.repo2.bucket}
          repo2-s3-endpoint = ${cfg.repo2.endpoint}
          repo2-s3-region = ${cfg.repo2.region}
          repo2-s3-uri-style = ${cfg.repo2.uriStyle}
          repo2-type = s3
        ''
        + ''
          [${stanza}]
          pg1-path = ${cfg.pgDataDir}
          pg1-socket-path = /run/postgresql
        '';
    };

    environment.systemPackages = with pkgs; [ pgbackrest ];

    services.postgresql = {
      enable = true;
      package = cfg.package;
      settings = {
        archive_mode = "on";
        archive_command = "${pkgs.pgbackrest}/bin/pgbackrest --stanza=${stanza} archive-push %p";
        max_wal_senders = 3;
        wal_level = "replica";
      };
    };

    environment.persistence."/persist" = mkIf withImpermanence {
      directories = [
        cfg.pgDataDir
        "/var/log/pgbackrest"
        "/var/spool/pgbackrest"
      ];
    };

    systemd.tmpfiles.rules =
      let
        prefix = if withImpermanence then "/persist" else "";
      in
      [
        "d ${prefix}${cfg.pgDataDir} 750 postgres postgres"
        "d ${prefix}/var/log/pgbackrest 750 postgres postgres"
        "d ${prefix}/var/spool/pgbackrest 750 postgres postgres"
      ];

    systemd.services.pgbackrest-diff-backup = {
      description = "pgBackRest Differential Backup";
      serviceConfig = {
        User = "postgres";
        Group = "postgres";
      };
      script = "${pkgs.pgbackrest}/bin/pgbackrest --type=diff --stanza=${stanza} backup";
    };

    systemd.services.pgbackrest-full-backup-repo1 = lib.mkIf cfg.repo1.enable (fullBackupService "1");
    systemd.timers.pgbackrest-full-backup-repo1 = lib.mkIf cfg.repo1.enable (
      timerBase cfg.repo1.timers.full "Full"
    );
    systemd.services.pgbackrest-diff-backup-repo1 = lib.mkIf cfg.repo1.enable (diffBackupService "1");
    systemd.timers.pgbackrest-diff-backup-repo1 = lib.mkIf cfg.repo1.enable (
      timerBase cfg.repo1.timers.diff "Diff"
    );
    systemd.services.pgbackrest-incr-backup-repo1 = lib.mkIf cfg.repo1.enable (incrBackupService "1");
    systemd.timers.pgbackrest-incr-backup-repo1 = lib.mkIf cfg.repo1.enable (
      timerBase cfg.repo1.timers.incr "Incr"
    );

    systemd.services.pgbackrest-full-backup-repo2 = lib.mkIf cfg.repo2.enable (fullBackupService "2");
    systemd.timers.pgbackrest-full-backup-repo2 = lib.mkIf cfg.repo2.enable (
      timerBase cfg.repo2.timers.full "Full"
    );
    systemd.services.pgbackrest-diff-backup-repo2 = lib.mkIf cfg.repo2.enable (diffBackupService "2");
    systemd.timers.pgbackrest-diff-backup-repo2 = lib.mkIf cfg.repo2.enable (
      timerBase cfg.repo2.timers.diff "Diff"
    );
    systemd.services.pgbackrest-incr-backup-repo2 = lib.mkIf cfg.repo2.enable (incrBackupService "2");
    systemd.timers.pgbackrest-incr-backup-repo2 = lib.mkIf cfg.repo2.enable (
      timerBase cfg.repo2.timers.incr "Incr"
    );

    systemd.services.pgbackrest-init = {
      enable = true;
      description = "pgBackRest initialization";
      serviceConfig = {
        User = "postgres";
        Group = "postgres";
        Type = "oneshot";
        RemainAfterExit = true;
      };
      script = ''
        ${pkgs.pgbackrest}/bin/pgbackrest --stanza=${stanza} --log-level-console=info stanza-create
        ${pkgs.pgbackrest}/bin/pgbackrest --stanza=${stanza} --log-level-console=info check
      '';
    };
  };
}
