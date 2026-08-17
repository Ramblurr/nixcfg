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
  nodePull = name: jobs.${name};
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
    "rpool<" = false;
    "rpool2<" = false;
    "tank/backup<" = false;
    "tank2<" = true;
    "tank2/iocage<" = false;
    "tank2/media<" = false;
    "tank2/media/music/mine" = true;
    "tank2/replication<" = false;
    "tank2/proxmox<" = false;
    "tank2/backups/gamsjaegers<" = false;
  };
  expectedOffsiteFilesystems = {
    "rpool<" = false;
    "rpool2<" = false;
    "tank/backup<" = false;
    "tank2<" = true;
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
assert
  builtins.attrNames jobs == [
    "debord"
    "dewey"
    "mali_rsyncnet"
    "mali_snap"
    "mali_source"
  ];
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
assert
  map (name: (nodePull name).pruning.keep_receiver) [
    "debord"
    "dewey"
  ] == [
    keepAll
    keepAll
  ];
assert
  map (name: (nodePull name).pruning.keep_sender) [
    "debord"
    "dewey"
  ] == [
    senderRetention
    senderRetention
  ];
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
assert jobs.mali_rsyncnet.filesystems == expectedOffsiteFilesystems;
assert !(jobs.mali_rsyncnet ? pruning);
assert jobs.mali_source.type == "source";
assert jobs.mali_source.snapshotting == { type = "manual"; };
assert !(jobs.mali_source ? pruning);
pkgs.runCommand "mali-zrepl-retention-test" { } ''
  touch "$out"
''
