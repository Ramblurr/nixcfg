{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.opencloud.maliStorage;
  guestAddress = builtins.head config.site.net.svc.hosts4.opencloud-home;
in
{
  options.modules.services.opencloud.maliStorage = lib.mkOption {
    type = lib.types.nullOr (
      lib.types.submodule (_: {
        options = {
          dataset = lib.mkOption {
            type = lib.types.str;
            description = "ZFS dataset containing this OpenCloud instance's data.";
          };
          dataDir = lib.mkOption {
            type = lib.types.str;
            description = "Mountpoint for this OpenCloud instance's data dataset.";
          };
        };
      })
    );
    default = null;
    description = "Mali storage and NFS export for one OpenCloud instance.";
  };

  config = lib.mkIf (cfg != null) {
    users.groups.opencloud-home.gid = 3100;
    users.users.opencloud-home = {
      isSystemUser = true;
      uid = 3100;
      group = "opencloud-home";
    };
    modules.zfs.datasets = {
      enable = true;
      properties.${cfg.dataset} = {
        mountpoint = cfg.dataDir;
        xattr = "sa";
      };
      services.${cfg.dataset} = [ "opencloud-home-nfs-path" ];
    };
    systemd.services.opencloud-home-nfs-path = {
      before = [ "nfs-server.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
      # Only the mounted dataset root changes ownership. OpenCloud creates its
      # users/metadata directories; never recursively chown application data.
      script = ''
        ${pkgs.coreutils}/bin/chown 3100:3100 ${cfg.dataDir}
        ${pkgs.coreutils}/bin/chmod 0700 ${cfg.dataDir}
      '';
    };
    systemd.services.nfs-server = {
      requires = [ "opencloud-home-nfs-path.service" ];
      after = [ "opencloud-home-nfs-path.service" ];
    };
    services.nfs.server.exports = lib.mkAfter ''
      ${cfg.dataDir} ${guestAddress}(rw,sync,root_squash,no_subtree_check)
    '';
  };
}
