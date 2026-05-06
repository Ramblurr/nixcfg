{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.modules.services.rclone.mounts;
  mountType = lib.types.submodule {
    options = {
      enable = lib.mkOption {
        type = lib.types.bool;
        description = "Whether to enable this rclone mount.";
        default = true;
      };
      user = lib.mkOption {
        type = lib.types.str;
        description = "User to mount the directory for";
      };
      group = lib.mkOption {
        type = lib.types.str;
        description = "Group to mount the directory for";
      };
      remote_name = lib.mkOption {
        type = lib.types.str;
        description = "Remote name to mount";
      };
      mount_point = lib.mkOption {
        type = lib.types.str;
        description = "Where to mount the directory";
      };
      extraOpts = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "Extra options to pass to rclone mount.";
        default = [ ];
      };
      dependent_service = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        description = "Service name to wait for the mount.";
        default = null;
      };
    };
  };

  enabledCfg = lib.filterAttrs (_: mcfg: mcfg.enable) cfg;

  mkConfigArg = mcfg: "--config ${config.users.users.${mcfg.user}.home}/.config/rclone/rclone.conf";

  mkExtraOpts = mcfg: lib.concatStringsSep " \
              " mcfg.extraOpts;

  mkTmpfileRule = _: mcfg: "d ${mcfg.mount_point} 0770 ${mcfg.user} ${mcfg.group} -";

  setDependentServices =
    name: mcfg:
    lib.optionalAttrs (mcfg.dependent_service != null) {
      ${mcfg.dependent_service} = {
        # FIXME DRY
        requires = [ "rclone-mount-${name}.service" ];
        after = [ "rclone-mount-${name}.service" ];
      };
    };

  mkRcloneService = name: mcfg: {
    "rclone-mount-${name}" = {
      description = "rclone mount ${mcfg.remote_name} to ${mcfg.mount_point}";
      requires = [ "network-online.target" ];
      after = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];

      # Prepend wrappers so rclone uses the setuid fusermount3. Without
      # this rclone won't be able to perform mount (requires setuid)
      environment.PATH = lib.mkForce "/run/wrappers/bin";
      serviceConfig = {
        ExecStart = ''
          ${pkgs.rclone}/bin/rclone mount ${mcfg.remote_name}: ${mcfg.mount_point} \
              ${mkConfigArg mcfg} \
              --allow-other \
              --uid ${toString config.users.users.${mcfg.user}.uid} \
              --gid ${toString config.users.groups.${mcfg.group}.gid} \
              --default-permissions \
              --umask 007 \
              --dir-perms 0770 \
              --file-perms 0660 \
              --dir-cache-time 10m \
              --vfs-cache-mode full \
              --vfs-cache-max-age 48h \
              --vfs-read-chunk-size 10M \
              --vfs-read-chunk-size-limit 512M \
              --no-modtime \
              --allow-non-empty \
              --buffer-size 512M \
              ${mkExtraOpts mcfg}'';
        ExecStop = "/run/wrappers/bin/fusermount -u ${mcfg.mount_point}";
        Type = "notify";
        User = mcfg.user;
        Group = mcfg.group;
        Restart = "always";
        RestartSec = "10s";
      };
    };
  };
in
{
  options.modules.services.rclone.mounts = lib.mkOption {
    type = lib.types.attrsOf mountType;
    default = { };
  };

  config = lib.mkIf (enabledCfg != { }) {
    systemd.tmpfiles.rules = lib.mapAttrsToList mkTmpfileRule enabledCfg;

    systemd.services = lib.mkMerge (
      lib.flatten [
        (lib.mapAttrsToList mkRcloneService enabledCfg)
        (lib.mapAttrsToList setDependentServices enabledCfg)
      ]
    );

    # required by --allow-other on rclone
    programs.fuse.userAllowOther = true;
  };
}
# TODO manage the SSH Key
