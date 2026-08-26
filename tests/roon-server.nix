{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  hostPkgs = import inputs.nixpkgs {
    system = pkgs.stdenv.hostPlatform.system;
    config.allowUnfree = true;
  };
  evaluated = inputs.nixpkgs.lib.nixosSystem {
    system = pkgs.stdenv.hostPlatform.system;
    modules = [
      ../modules/zfs-attrs.nix
      ../modules/services/roon-server.nix
      {
        nixpkgs.pkgs = hostPkgs;
        system.stateVersion = "26.05";
        boot.loader.grub.devices = [ "nodev" ];
        fileSystems."/" = {
          device = "none";
          fsType = "tmpfs";
        };
        modules.zfs.datasets.enable = true;
        modules.services.roon-server.enable = true;
      }
    ];
  };
  service = evaluated.config.systemd.services.roon-server;
  dependencyNames = service.requires ++ service.wants ++ service.after ++ service.bindsTo;
  nfsMounts = [
    "mnt-roon-backup.mount"
    "mnt-roon-music\\x2dother.mount"
    "mnt-roon-music\\x2dmine.mount"
    "mnt-roon-audiobooks.mount"
  ];
in
assert builtins.elem "zfs-datasets.service" service.requires;
assert builtins.elem "zfs-datasets.service" service.after;
assert builtins.elem "zfs-mount.service" service.bindsTo;
assert !(builtins.elem "var-lib-private-roon\\x2dserver.mount" dependencyNames);
assert lib.all (
  mount: builtins.elem mount service.after && builtins.elem mount service.bindsTo
) nfsMounts;
assert service.unitConfig.AssertPathIsMountPoint == [ "/var/lib/private/roon-server" ];
assert builtins.elem "/var/lib/private/roon-server" service.unitConfig.RequiresMountsFor;
pkgs.runCommand "roon-server-evaluation" { } "touch $out"
