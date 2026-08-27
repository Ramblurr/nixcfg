{ inputs, pkgs }:
let
  mkEvaluated =
    datasetConfig:
    inputs.nixpkgs.lib.nixosSystem {
      system = pkgs.stdenv.hostPlatform.system;
      modules = [
        ../modules/zfs-attrs.nix
        {
          nixpkgs.pkgs = pkgs;
          system.stateVersion = "26.05";
          boot.loader.grub.devices = [ "nodev" ];
          fileSystems."/" = {
            device = "none";
            fsType = "tmpfs";
          };
          modules.zfs.datasets = datasetConfig;
        }
      ];
    };
  validConfig = {
    enable = true;
    properties = {
      "tank/services/alpha".mountpoint = "/srv/alpha";
      "tank/services/shared".mountpoint = "/srv/shared";
    };
    services = {
      "tank/services/alpha" = [
        "alpha"
        "shared"
      ];
      "tank/services/shared" = [ "shared" ];
    };
  };
  evaluated = mkEvaluated validConfig;
  invalidDataset = builtins.tryEval (mkEvaluated (
      validConfig
      // {
        services."tank/services/missing" = [ "broken" ];
      }
    )).config.system.build.toplevel.drvPath;
  invalidMountpoint = builtins.tryEval (mkEvaluated (
      validConfig
      // {
        properties."tank/services/alpha".mountpoint = "legacy";
      }
    )).config.system.build.toplevel.drvPath;
  invalidService = builtins.tryEval (mkEvaluated (
      validConfig
      // {
        services."tank/services/alpha" = [ "broken.service" ];
      }
    )).config.system.build.toplevel.drvPath;
  service = evaluated.config.systemd.services.zfs-datasets;
  alpha = evaluated.config.systemd.services.alpha;
  shared = evaluated.config.systemd.services.shared;
in
assert !invalidDataset.success;
assert !invalidMountpoint.success;
assert !invalidService.success;
assert builtins.elem "zfs-mount.service" service.requires;
assert builtins.elem "zfs-mount.service" service.after;
assert service.serviceConfig.Type == "oneshot";
assert service.serviceConfig.RemainAfterExit;
assert builtins.elem "zfs-datasets.service" alpha.requires;
assert builtins.elem "zfs-datasets.service" alpha.after;
assert builtins.elem "zfs-mount.service" alpha.bindsTo;
assert alpha.unitConfig.AssertPathIsMountPoint == [ "/srv/alpha" ];
assert alpha.unitConfig.RequiresMountsFor == [ "/srv/alpha" ];
assert
  shared.unitConfig.AssertPathIsMountPoint == [
    "/srv/alpha"
    "/srv/shared"
  ];
assert
  shared.unitConfig.RequiresMountsFor == [
    "/srv/alpha"
    "/srv/shared"
  ];
pkgs.runCommand "zfs-datasets-evaluation" { } "touch $out"
