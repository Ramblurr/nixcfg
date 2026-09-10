{ inputs, pkgs }:
let
  lib = inputs.nixpkgs.lib;
  evaluate =
    available: snapshots:
    (lib.nixosSystem {
      specialArgs = {
        inherit inputs;
        actual-nixpkgs = inputs.nixpkgs;
      };
      modules = [
        ../modules/site/gatus.nix
        ../modules/site/gatus-heartbeats.nix
        ../modules/nix.nix
        ../modules/nix-prune-aged-gcroots.nix
        ../modules/boot/zfs.nix
        ../modules/services/zfs-backup-check.nix
        ({ lib, ... }: {
          options = {
            repo.secrets = lib.mkOption { type = lib.types.attrs; };
            modules.impermanence.enable = lib.mkOption {
              type = lib.types.bool;
              default = false;
            };
            modules.server.smtp-external-relay.enable = lib.mkOption {
              type = lib.types.bool;
              default = false;
            };
            environment.persistence = lib.mkOption {
              type = lib.types.attrs;
              default = { };
            };
          };
          config = {
            nixpkgs.pkgs = pkgs;
            networking.hostName = "test";
            system.stateVersion = "26.05";
            repo.secrets.global = {
              domain.home = "example.test";
              ciSigningPublicKey = "test";
            };
            site.gatus.heartbeatToken.environmentFile = if available then "/run/secrets/gatus-env" else null;
            modules.nix.pruneAgedGcroots.enable = true;
            modules.boot.zfs = {
              enable = true;
              autoSnapshot.enable = snapshots;
              trimPools = [ "ssd" ];
            };
            boot.supportedFilesystems = [ "zfs" ];
            modules.services.zfs-backup-check = {
              enable = available;
              healthchecks = [
                {
                  dataset = "recent";
                  time = "1 hour ago";
                }
                {
                  dataset = "stale";
                  time = "1 hour ago";
                }
              ];
            };
          };
        })
      ];
    }).config;
  cfg = evaluate true true;
  noToken = evaluate false true;
  noSnapshots = evaluate true false;
  cleanup = pkgs.writeShellScript "cleanup-test" (
    lib.replaceStrings [ "/nix/var/nix" ] [ "$TMPDIR/nix" ]
      cfg.systemd.services.nix-cleanup-gcroots.script
  );
  prune = pkgs.writeShellScript "prune-test" (
    lib.replaceStrings [ "/nix/var/nix" ] [ "$TMPDIR/nix" ]
      cfg.systemd.services.nix-prune-aged-gcroots.script
  );
  zfsFixture = pkgs.writeShellScriptBin "zfs" ''
    case "''${!#}" in
      recent) date +%s ;;
      stale) echo 1 ;;
      missing) ;;
      invalid) echo invalid ;;
      error) exit 42 ;;
    esac
  '';
  check = pkgs.callPackage ../pkgs/zfs-snapshot-age.nix { zfs = zfsFixture; };
  productionCheck = pkgs.callPackage ../pkgs/zfs-snapshot-age.nix { };
  backup = pkgs.writeShellScript "backup-test" (
    lib.replaceStrings [ (lib.getExe productionCheck) ] [ (lib.getExe check) ]
      cfg.systemd.services.zrepl-healthcheck.script
  );
  trimFixture = pkgs.writeShellScriptBin "zpool" ''
    test "$1" = trim && test "$2" = -w || exit 99
    test "$3" != failed
  '';
  trim =
    pools:
    pkgs.callPackage ../pkgs/zpool-trim.nix {
      zfs = trimFixture;
      inherit pools;
    };
in
assert
  builtins.attrNames cfg.site.gatus.heartbeats == [
    "nix-cleanup-gcroots"
    "nix-gc"
    "nix-prune-aged-gcroots"
    "zfs-scrub"
    "zfs-snapshot-daily"
    "zfs-snapshot-frequent"
    "zfs-snapshot-hourly"
    "zfs-snapshot-monthly"
    "zfs-snapshot-weekly"
    "zpool-trim"
    "zrepl-healthcheck"
  ];
assert noToken.site.gatus.heartbeats == { };
assert !(noSnapshots.site.gatus.heartbeats ? zfs-snapshot-frequent);
assert cfg.systemd.services.zfs-scrub.serviceConfig.Type == "oneshot";
assert cfg.systemd.services.zfs-scrub.serviceConfig.TimeoutStartSec == "infinity";
assert cfg.systemd.services.zpool-trim.serviceConfig.Type == "oneshot";
assert lib.hasInfix "--success true" (
  lib.last cfg.systemd.services.zrepl-healthcheck.serviceConfig.ExecStartPost
);
assert !(lib.hasInfix "curl" cfg.systemd.services.zrepl-healthcheck.script);
pkgs.runCommand "maintenance-heartbeats-test" { nativeBuildInputs = [ pkgs.coreutils ]; } ''
  mkdir -p "$TMPDIR/nix/temproots" "$TMPDIR/nix/gcroots"
  touch "$TMPDIR/nix/temproots/fresh" "$TMPDIR/nix/temproots/old"
  touch -d '11 days ago' "$TMPDIR/nix/temproots/old"
  ln -s "$TMPDIR/nix/temproots/fresh" "$TMPDIR/nix/gcroots/live"
  ln -s "$TMPDIR/absent" "$TMPDIR/nix/gcroots/broken"
  ${cleanup}
  test -f "$TMPDIR/nix/temproots/fresh"
  test ! -e "$TMPDIR/nix/temproots/old"
  test -L "$TMPDIR/nix/gcroots/live"
  test ! -L "$TMPDIR/nix/gcroots/broken"
  rm -r "$TMPDIR/nix/temproots"
  if ${cleanup}; then echo 'cleanup concealed missing input'; exit 1; fi

  ${prune} # Optional directories need not exist yet.
  mkdir -p "$TMPDIR/nix/gcroots/auto"
  ln -s "$TMPDIR/target" "$TMPDIR/nix/gcroots/auto/old"
  ln -s "$TMPDIR/target" "$TMPDIR/nix/gcroots/auto/fresh"
  touch -h -d '91 days ago' "$TMPDIR/nix/gcroots/auto/old"
  ${prune}
  test ! -L "$TMPDIR/nix/gcroots/auto/old"
  test -L "$TMPDIR/nix/gcroots/auto/fresh"

  ${lib.getExe check} recent '1 hour ago'
  for dataset in stale missing invalid error; do
    if ${lib.getExe check} "$dataset" '1 hour ago'; then
      echo "incorrect success for $dataset"; exit 1
    fi
  done
  if ${lib.getExe check} recent 'not a date'; then exit 1; fi
  if ${backup}; then echo 'aggregate concealed stale dataset'; exit 1; fi
  ${lib.getExe (trim [
    "ssd"
    "other"
  ])}
  if ${
    lib.getExe (trim [
      "failed"
      "ssd"
    ])
  }; then echo 'trim concealed first pool failure'; exit 1; fi
  if ${
    lib.getExe (trim [
      "ssd"
      "failed"
    ])
  }; then echo 'trim concealed last pool failure'; exit 1; fi
  touch "$out"
''
