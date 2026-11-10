{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  keepAll = [
    {
      type = "regex";
      regex = ".*";
    }
  ];
  senderRetention = [
    { type = "not_replicated"; }
    {
      type = "last_n";
      count = 7;
    }
    {
      type = "grid";
      grid = "1x24h(keep=all) | 7x1d(keep=1) | 30x1d(keep=1) | 6x30d(keep=1) | 1x365d(keep=1)";
      regex = "^zrepl_.*";
    }
    {
      type = "regex";
      negate = true;
      regex = "^zrepl_.*";
    }
  ];
  secretFile = pkgs.writeText "mali-zrepl-retention-test-secrets.yaml" "{}\n";
  evaluated = inputs.nixpkgs.lib.nixosSystem {
    system = pkgs.stdenv.hostPlatform.system;
    modules = [
      inputs.sops-nix.nixosModules.sops
      ../hosts/mali/zrepl.nix
      (
        { lib, ... }:
        {
          options.repo.secrets = lib.mkOption { type = lib.types.attrs; };
          options.modules.services.zfs-backup-check = lib.mkOption {
            type = lib.types.attrs;
            default = { };
          };
        }
      )
      {
        nixpkgs.pkgs = pkgs;
        system.stateVersion = "26.05";
        boot.loader.grub.devices = [ "nodev" ];
        fileSystems."/" = {
          device = "none";
          fsType = "tmpfs";
        };
        networking.hostName = "mali";
        repo.secrets = {
          global.nodes = {
            debord = {
              dataCIDR = "192.0.2.10/24";
              zreplSource = true;
            };
            dewey = {
              dataCIDR = "192.0.2.11/24";
              zreplSource = true;
            };
            ignored = {
              dataCIDR = "192.0.2.12/24";
              zreplSource = false;
            };
          };
          home-ops.ports = {
            zrepl-metrics = 9811;
            zrepl-source = 9812;
          };
          local.zfsHealthchecks = [ ];
        };
        sops.defaultSopsFile = secretFile;
        sops.age.keyFile = "/tmp/age-key.txt";
      }
    ];
  };
  jobs = lib.listToAttrs (
    map (job: lib.nameValuePair job.name job) evaluated.config.services.zrepl.settings.jobs
  );
  pullJobs = lib.filter (job: job.type == "pull") evaluated.config.services.zrepl.settings.jobs;
  offsiteConfig = (pkgs.formats.yaml { }).generate "mali-offsite-filter-test.yml" {
    jobs = [ jobs.mali_rsyncnet ];
  };
  cidrToIp = ip: builtins.head (builtins.split "/" ip);
  sourceLib = lib // {
    my = { inherit cidrToIp; };
    mkIf = condition: value: if condition then value else { };
  };
  sourceJob =
    name:
    builtins.head
      (import ../config/zrepl.nix {
        lib = sourceLib;
        config = {
          networking.hostName = name;
          home-ops.zrepl.enable = true;
          repo.secrets = {
            global.nodes = {
              ${name}.dataCIDR = "192.0.2.20/24";
              mali.dataCIDR = "192.0.2.1/24";
            };
            home-ops.ports = {
              zrepl-metrics = 9811;
              zrepl-source = 9812;
            };
          };
        };
      }).config.services.zrepl.settings.jobs;
  expectedMaliSnapFilesystems = {
    "fast<" = false;
    "fast/services/garage/metadata" = true;
    "rpool<" = false;
    "rpool2<" = false;
    "tank/backup<" = false;
    "tank2<" = true;
    "tank2/services/garage<" = true;
    "tank2/iocage<" = false;
    "tank2/media<" = false;
    "tank2/media/music/mine" = true;
    "tank2/replication<" = false;
    "tank2/proxmox<" = false;
    "tank2/backups/gamsjaegers<" = false;
  };
  expectedOffsiteFilesystems = {
    "fast<" = false;
    "fast/services/garage/metadata" = true;
    "rpool<" = false;
    "rpool2<" = false;
    "tank/backup<" = false;
    "tank2<" = true;
    "tank2/services/garage<" = true;
    "tank2/media<" = false;
    "tank2/media/music/mine" = true;
    "tank2/replication<" = true;
    "tank2/replication/dewey<" = false;
    "tank2/replication/dewey/rpool/encrypted/safe/extra/atuin<" = true;
    "tank2/replication/dewey/rpool/encrypted/safe/persist<" = true;
    "tank2/replication/dewey/rpool/encrypted/safe/svc<" = true;
    "tank2/replication/dewey/rpool/encrypted/safe/vms<" = true;
    "tank2/proxmox<" = false;
    "tank2/backups/gamsjaegers<" = false;
  };
in
# Fixture sources must be pulled, but unrelated jobs may be added independently.
assert lib.all (name: builtins.any (job: job.name == name) pullJobs) [
  "debord"
  "dewey"
];
assert !(jobs ? ignored);
assert
  map
    (
      name:
      let
        job = sourceJob name;
      in
      {
        inherit (job)
          name
          type
          filesystems
          snapshotting
          ;
      }
    )
    [
      "debord"
      "dewey"
    ] == [
    {
      name = "debord_source";
      type = "source";
      filesystems = {
        "rpool<" = false;
        "rpool/encrypted/safe/svc<" = true;
        "rpool/encrypted/safe/persist<" = true;
        "rpool/encrypted/safe/vms<" = true;
        "rpool/encrypted/safe/extra/atuin<" = true;
        "tank<" = false;
      };
      snapshotting = {
        type = "periodic";
        prefix = "zrepl_";
        interval = "6h";
      };
    }
    {
      name = "dewey_source";
      type = "source";
      filesystems = {
        "rpool<" = false;
        "rpool/encrypted/safe/svc<" = true;
        "rpool/encrypted/safe/persist<" = true;
        "rpool/encrypted/safe/vms<" = true;
        "rpool/encrypted/safe/extra/atuin<" = true;
        "tank<" = false;
      };
      snapshotting = {
        type = "periodic";
        prefix = "zrepl_";
        interval = "6h";
      };
    }
  ];
# Every pull must preserve the offsite queue and protect unreplicated snapshots.
assert lib.assertMsg (lib.all
  (job: job.pruning.keep_receiver == keepAll && job.pruning.keep_sender == senderRetention)
  pullJobs
) "Mali pull jobs must retain all received snapshots and apply the sender retention policy";
assert
  jobs.mali_snap.snapshotting == {
    type = "periodic";
    prefix = "zrepl_";
    interval = "6h";
  };
assert jobs.mali_snap.filesystems == expectedMaliSnapFilesystems;
assert jobs.mali_snap.pruning.keep == keepAll;
assert jobs.mali_rsyncnet.type == "source";
assert jobs.mali_rsyncnet.snapshotting == { type = "manual"; };
assert
  lib.intersectAttrs expectedOffsiteFilesystems jobs.mali_rsyncnet.filesystems
  == expectedOffsiteFilesystems;
assert !(jobs.mali_rsyncnet ? pruning);
assert jobs.mali_source.type == "source";
assert jobs.mali_source.snapshotting == { type = "manual"; };
assert jobs.mali_source.filesystems."fast<" == false;
assert jobs.mali_source.filesystems."fast/services/garage/metadata" == true;
assert jobs.mali_source.filesystems."tank2/services/garage<" == true;
assert !(jobs.mali_source ? pruning);
pkgs.runCommand "mali-zrepl-retention-test" { nativeBuildInputs = [ pkgs.zrepl ]; } ''
  check_filter() {
    expected=$1
    dataset=$2
    actual=$(zrepl --config ${offsiteConfig} test filesystems --job mali_rsyncnet --input "$dataset" | cut -f1)
    if [ "$actual" != "$expected" ]; then
      echo "$dataset: expected $expected, got $actual" >&2
      exit 1
    fi
  }

  # Test real filter behavior, not the spelling or number of filter rules.
  root=tank2/replication/peirce
  for dataset in "$root" "$root/rpool" "$root/rpool/encrypted" "$root/rpool/other"; do
    check_filter REJECT "$dataset"
  done
  for suffix in persist svc svc/forgejo svc/dependency-mirror svc/new-service vms; do
    check_filter ACCEPT "$root/rpool/encrypted/safe/$suffix"
  done
  touch "$out"
''
