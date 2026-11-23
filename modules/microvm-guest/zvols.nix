{ config, lib, ... }:
let
  cfg = config.modules.microvm-guest;
  inherit (cfg) zvols;
  runner = config.microvm.runner.${config.microvm.hypervisor};
in
{
  options.modules.microvm-guest.zvols = lib.mkOption {
    description = ''
      Persistent ext4 volumes, each backed by a sparse ZFS zvol on the host.
      The attribute name names the zvol and labels its filesystem.
      The host's microvm-zfs-datasets@ unit creates and formats each zvol once,
      reading the sizes from the runner's share/microvm/zvols.
    '';
    default = { };
    type = lib.types.attrsOf (
      lib.types.submodule {
        options = {
          size = lib.mkOption {
            type = lib.types.ints.positive;
            description = "Size in MiB. The host only uses it when it creates the zvol.";
          };
          mountPoint = lib.mkOption {
            type = lib.types.path;
            description = "Where the guest mounts the volume.";
          };
        };
      }
    );
  };

  config = lib.mkIf (cfg.enable && zvols != { }) {
    assertions = lib.mapAttrsToList (name: _: {
      assertion = builtins.match "[a-z0-9][a-z0-9-]{0,15}" name != null;
      message = "MicroVM zvol name ${name} must be a valid ext4 label: at most 16 of [a-z0-9-].";
    }) zvols;

    microvm.volumes = lib.mapAttrsToList (name: zvol: {
      image = "${cfg.mountBase}/${name}.zvol";
      inherit (zvol) size;
      autoCreate = false;
      # Mounted by label below, so microvm.nix does not warn about unlabeled volumes.
      mountPoint = null;
    }) zvols;

    fileSystems = lib.mapAttrs' (
      name: zvol:
      lib.nameValuePair zvol.mountPoint {
        device = "/dev/disk/by-label/${name}";
        fsType = "ext4";
        neededForBoot = true;
      }
    ) zvols;

    microvm.declaredRunner = runner.overrideAttrs (old: {
      buildCommand =
        old.buildCommand
        + lib.concatStrings (
          lib.mapAttrsToList (name: zvol: ''
            mkdir -p $out/share/microvm/zvols/${name}
            echo ${toString zvol.size} > $out/share/microvm/zvols/${name}/size
          '') zvols
        );
    });
  };
}
