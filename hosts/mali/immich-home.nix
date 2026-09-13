{
  config,
  lib,
  pkgs,
  utils,
  ...
}:
let
  instance = import ../../config/immich-home.nix;
  address = network: host: builtins.head config.site.net.${network}.hosts4.${host};
  exportClient =
    network: host: "${address network host}(rw,sync,root_squash,no_subtree_check,crossmnt)";
  derivedMounts =
    lib.genAttrs (map (dir: "${instance.mediaExport}/${dir}") instance.derivedDirectories)
      (mountpoint: "${instance.derivedDataset}/${builtins.baseNameOf mountpoint}");
  mountUnits = map (path: "${utils.escapeSystemdPath path}.mount") (builtins.attrNames derivedMounts);
  # Run before either dataset creation or reactivation can hide unmigrated data.
  migrationGuard = lib.concatStringsSep "\n" (
    lib.mapAttrsToList (mountpoint: dataset: ''
      if ${pkgs.util-linux}/bin/mountpoint -q ${lib.escapeShellArg mountpoint}; then
        test "$(${pkgs.util-linux}/bin/findmnt -n -o SOURCE --mountpoint ${lib.escapeShellArg mountpoint})" = ${lib.escapeShellArg dataset}
      elif test -L ${lib.escapeShellArg mountpoint} || { test -d ${lib.escapeShellArg mountpoint} && test -n "$(${pkgs.findutils}/bin/find ${lib.escapeShellArg mountpoint} -mindepth 1 -maxdepth 1 -print -quit)"; }; then
        echo "Immich derived media must be migrated before activation: ${mountpoint}" >&2
        exit 1
      fi
    '') derivedMounts
  );
in
{
  modules.zfs.datasets = {
    properties = {
      ${instance.mediaDataset} = {
        mountpoint = instance.mediaExport;
        compression = "zstd";
        atime = "off";
      };
      ${instance.derivedDataset} = {
        mountpoint = "none";
        canmount = "off";
        compression = "zstd";
        atime = "off";
      };
    }
    // lib.listToAttrs (
      map (
        dataset:
        lib.nameValuePair dataset {
          # Explicit systemd mounts provide ordering across pools and loss dependencies.
          mountpoint = "legacy";
          canmount = "noauto";
          overlay = "off";
        }
      ) (builtins.attrValues derivedMounts)
    );
    services.${instance.mediaDataset} = [
      "immich-home-media-setup"
      "nfs-server"
    ];
  };
  systemd.mounts = lib.mapAttrsToList (mountpoint: dataset: {
    where = mountpoint;
    what = dataset;
    type = "zfs";
    # Pulled in by NFS after dataset creation, not by local-fs.target.
    requires = [ "zfs-datasets.service" ];
    after = [ "zfs-datasets.service" ];
    before = [ "umount.target" ];
    conflicts = [ "umount.target" ];
    unitConfig = {
      DefaultDependencies = false;
      AssertPathIsMountPoint = instance.mediaExport;
      AssertDirectoryNotEmpty = "!${mountpoint}";
    };
  }) derivedMounts;

  users.users.immich = {
    isSystemUser = true;
    inherit (instance) uid;
    group = "immich";
  };
  users.groups.immich.gid = instance.gid;

  systemd.services.zfs-datasets.preStart = lib.mkBefore migrationGuard;
  systemd.services.zfs-datasets-reactivation.preStart = lib.mkBefore migrationGuard;
  systemd.services.nfs-server = {
    bindsTo = mountUnits;
    after = mountUnits;
  };
  systemd.services.immich-home-media-setup = {
    requiredBy = [ "nfs-server.service" ];
    before = [ "nfs-server.service" ];
    bindsTo = mountUnits;
    after = mountUnits;
    unitConfig.AssertPathIsMountPoint = [ instance.mediaExport ] ++ builtins.attrNames derivedMounts;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    script = lib.concatMapStringsSep "\n" (mountpoint: ''
      ${pkgs.coreutils}/bin/install -d -m 0700 -o ${toString instance.uid} -g ${toString instance.gid} ${lib.escapeShellArg mountpoint}
    '') ([ instance.mediaExport ] ++ builtins.attrNames derivedMounts);
  };
  services.nfs.server.exports = lib.mkAfter ''
    ${instance.mediaExport} ${exportClient "data" "immich-home"} ${exportClient instance.workerNetwork instance.workerHost}
  '';
}
