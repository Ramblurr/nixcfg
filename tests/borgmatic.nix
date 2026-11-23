{ inputs, pkgs }:
let
  lib = inputs.nixpkgs.lib;
  groups = import ../modules/site/gatus-groups.nix;
  secretFile = pkgs.writeText "borgmatic-test-secrets.yaml" "{}\n";
  hostName = "quine";
  gatusUrl = "https://status.example.test";
  heartbeat = lib.getExe (pkgs.callPackage ../pkgs/gatus-heartbeat.nix { });
  startFile = "/run/borgmatic/gatus-start";
  evaluate =
    {
      enable,
      impermanence ? false,
      persistHomes ? false,
    }:
    lib.nixosSystem {
      modules = [
        inputs.sops-nix.nixosModules.sops
        inputs.impermanence.nixosModules.impermanence
        ../modules/impermanence/default.nix
        ../modules/services/onepassword-systemd-credentials.nix
        ../modules/site/gatus.nix
        ../modules/site/gatus-heartbeats.nix
        ../modules/site/gatus-heartbeats-onepassword.nix
        ../modules/services/borgmatic.nix
        {
          options.repo.secrets = lib.mkOption { type = lib.types.attrs; };
          options.modules.users.primaryUser.username = lib.mkOption {
            type = lib.types.str;
            default = "testuser";
          };
        }
        {
          nixpkgs.pkgs = pkgs;
          networking.hostName = hostName;
          system.stateVersion = "26.05";
          boot.loader.grub.devices = [ "nodev" ];
          fileSystems."/" = {
            device = "none";
            fsType = "tmpfs";
          };
          sops.defaultSopsFile = secretFile;
          sops.age.keyFile = "/tmp/age-key.txt";
          repo.secrets.global.domain.home = "example.test";
          modules.impermanence = {
            enable = impermanence;
            inherit persistHomes;
          };
          modules.services.borgmatic = lib.mkIf enable {
            enable = true;
            name = "test-backup";
            repositories = [
              {
                label = "test";
                path = "/backup";
              }
            ];
          };
        }
      ];
    };
  enabled = (evaluate { enable = true; }).config;
  disabled = (evaluate { enable = false; }).config;
  impermanentSplitHome =
    (evaluate {
      enable = true;
      impermanence = true;
      persistHomes = false;
    }).config;
  impermanentFullHome =
    (evaluate {
      enable = true;
      impermanence = true;
      persistHomes = true;
    }).config;
  persistedDirs =
    cfg: map (entry: entry.directory) (cfg.environment.persistence."/persist".directories or [ ]);
  splitHomeDirs = persistedDirs impermanentSplitHome;
  fullHomeDirs = persistedDirs impermanentFullHome;
  rootConfigBorgEntry = lib.findFirst (entry: entry.directory == "/root/.config/borg") null (
    impermanentSplitHome.environment.persistence."/persist".directories or [ ]
  );
  date = lib.getExe' pkgs.coreutils "date";
  expectedCommands = [
    {
      before = "action";
      when = [ "create" ];
      run = [ "${date} +%s > ${startFile}" ];
    }
    {
      after = "action";
      when = [ "create" ];
      states = [ "finish" ];
      run = [
        ''${heartbeat} report --url "${gatusUrl}" --group "${groups.infrastructure}" --name "Borgmatic Backup (${hostName})" --success true --duration "$(( $(${date} +%s) - $(cat ${startFile}) ))s"''
      ];
    }
    {
      after = "error";
      when = [ "create" ];
      run = [
        ''${heartbeat} report --url "${gatusUrl}" --group "${groups.infrastructure}" --name "Borgmatic Backup (${hostName})" --success false --error {error}''
      ];
    }
  ];
  timerUnit = enabled.systemd.units."borgmatic.timer".text;
  sshCommand = enabled.services.borgmatic.settings.ssh_command;
  matchArchives = enabled.services.borgmatic.settings.match_archives;
  archiveNameFormat = enabled.services.borgmatic.settings.archive_name_format;
  pruneConfig = (pkgs.formats.yaml { }).generate "borgmatic-prune-test.yaml" {
    repositories = [
      {
        label = "local";
        path = "__REPO__";
      }
    ];
    source_directories = [ "__SRC__" ];
    archive_name_format = archiveNameFormat;
    match_archives = matchArchives;
    keep_daily = 1;
  };
in
assert enabled.services.borgmatic.settings.commands == expectedCommands;
assert !(enabled.services.borgmatic.settings ? healthchecks);
assert enabled.systemd.services.borgmatic.serviceConfig.RuntimeDirectory == "borgmatic";
assert enabled.systemd.services.borgmatic.environment.BORG_USE_CHUNKS_ARCHIVE == "no";
assert
  enabled.systemd.timers.borgmatic.timerConfig.OnCalendar == [
    ""
    "*-*-* 04:00:00"
  ];
assert lib.hasInfix "OnCalendar=\nOnCalendar=*-*-* 04:00:00\n" timerUnit;
assert lib.hasInfix "StrictHostKeyChecking=accept-new" sshCommand;
assert !(lib.hasInfix "StrictHostKeyChecking=yes" sshCommand);
assert !(enabled.environment.persistence ? "/persist");
assert builtins.elem "/var/lib/borgmatic" splitHomeDirs;
assert builtins.elem "/root/.config/borg" splitHomeDirs;
assert rootConfigBorgEntry != null && rootConfigBorgEntry.mode == "0700";
assert !(builtins.elem "/root/.cache/borg" splitHomeDirs);
assert builtins.elem "/var/lib/borgmatic" fullHomeDirs;
assert !(builtins.elem "/root/.config/borg" fullHomeDirs);
assert !(builtins.elem "/root/.cache/borg" fullHomeDirs);
assert
  enabled.site.gatus.externalEndpoints == [
    {
      name = "Borgmatic Backup (${hostName})";
      group = groups.infrastructure;
      token = "$GATUS_EXTERNAL_TOKEN";
      heartbeat.interval = "30h";
      alerts = [ { type = "pushover"; } ];
    }
  ];
assert disabled.site.gatus.externalEndpoints == [ ];
pkgs.runCommand "borgmatic-test"
  {
    nativeBuildInputs = [
      pkgs.borgbackup
      pkgs.borgmatic
    ];
  }
  ''
    export HOME="$TMPDIR/home"
    mkdir -p "$HOME" "$TMPDIR/src" "$TMPDIR/repo"
    echo "hello" > "$TMPDIR/src/data.txt"

    sed \
      -e "s|__REPO__|$TMPDIR/repo|g" \
      -e "s|__SRC__|$TMPDIR/src|g" \
      ${pruneConfig} > "$TMPDIR/config.yaml"

    borg init --encryption=none "$TMPDIR/repo"
    borg create --timestamp 2026-01-01T04:00:00 "$TMPDIR/repo::test-backup-2026-01-01T04:00:00.000000" "$TMPDIR/src"
    borg create --timestamp 2026-01-02T04:00:00 "$TMPDIR/repo::test-backup-2026-01-02T04:00:00.000000" "$TMPDIR/src"

    borgmatic --config "$TMPDIR/config.yaml" config validate
    borgmatic --config "$TMPDIR/config.yaml" prune --verbosity 1

    remaining="$(borg list --short "$TMPDIR/repo")"
    if [ "$remaining" != "test-backup-2026-01-02T04:00:00.000000" ]; then
      echo "Expected prune to keep only the latest archive, got:" >&2
      printf '%s\n' "$remaining" >&2
      exit 1
    fi

    touch "$out"
  ''
