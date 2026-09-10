{
  config,
  lib,
  pkgs,
  ...
}:
let
  instance = import ../../config/immich-home.nix;
  address = host: builtins.head config.site.net.data.hosts4.${host};
  exportClient = host: "${address host}(rw,sync,root_squash,no_subtree_check)";
in
{
  modules.zfs.datasets = {
    properties.${instance.mediaDataset} = {
      mountpoint = instance.mediaExport;
      compression = "zstd";
      atime = "off";
    };
    services.${instance.mediaDataset} = [
      "immich-home-media-setup"
      "nfs-server"
    ];
  };
  users.users.immich = {
    isSystemUser = true;
    uid = instance.uid;
    group = "immich";
  };
  users.groups.immich.gid = instance.gid;

  systemd.services.immich-home-media-setup = {
    requiredBy = [ "nfs-server.service" ];
    before = [ "nfs-server.service" ];
    unitConfig.AssertPathIsMountPoint = instance.mediaExport;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    script = ''
      ${pkgs.coreutils}/bin/install -d -m 0700 -o ${toString instance.uid} -g ${toString instance.gid} ${lib.escapeShellArg instance.mediaExport}
    '';
  };
  services.nfs.server.exports = lib.mkAfter ''
    ${instance.mediaExport} ${exportClient "immich-home"} ${exportClient instance.workerHost}
  '';
}
