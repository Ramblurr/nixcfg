{ inputs, pkgs }:
let
  evaluated = inputs.nixpkgs.lib.nixosSystem {
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
        modules.zfs.datasets.enable = true;
      }
    ];
  };
  service = evaluated.config.systemd.services.zfs-datasets;
in
assert builtins.elem "zfs-mount.service" service.requires;
assert builtins.elem "zfs-mount.service" service.after;
assert service.serviceConfig.Type == "oneshot";
assert service.serviceConfig.RemainAfterExit;
pkgs.runCommand "zfs-datasets-evaluation" { } "touch $out"
