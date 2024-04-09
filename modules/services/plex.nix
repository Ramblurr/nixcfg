{
  options,
  config,
  lib,
  utils,
  pkgs,
  inputs,
  unstable,
  mine,
  ...
}:
let
  cfg = config.modules.services.plex;
  home-ops = config.repo.secrets.home-ops;
  localPath = "/mnt/mali/${cfg.nfsShare}";
  serviceDeps = [ "${utils.escapeSystemdPath localPath}.mount" ];
in
{
  options.modules.services.plex = {
    enable = lib.mkEnableOption "plex";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "plex.example.com";
      description = "The domain to use for the plex";
    };
    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
    };
    nfsShare = lib.mkOption { type = lib.types.str; };
    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption { type = lib.types.unspecified; };
  };
  config = lib.mkIf cfg.enable {
    users.users.${cfg.user.name} = {
      name = cfg.user.name;
      uid = lib.mkForce cfg.user.uid;
      isSystemUser = true;
      group = lib.mkForce cfg.group.name;
      extraGroups = [ "media" ];
    };

    users.groups.${cfg.group.name} = {
      name = cfg.group.name;
      gid = lib.mkForce cfg.group.gid;
    };

    fileSystems."${localPath}" = {
      device = "${config.repo.secrets.global.nodes.mali.data}:/mnt/${cfg.nfsShare}";
      fsType = "nfs";
    };

    modules.zfs.datasets.properties = {
      "tank/encrypted/svc/plex"."mountpoint" = config.services.plex.dataDir;
      "tank/encrypted/svc/plex"."com.sun:auto-snapshot" = "false";
    };

    systemd.services.plex.serviceConfig = {
      LockPersonality = true;
      NoNewPrivileges = true;
      DeviceAllow = [
        "char-drm rw"
        "/dev/dri rwm"
      ];
      PrivateUsers = true;
      PrivateTmp = true;
      ProtectClock = true;
      ProtectControlGroups = true;
      ProtectHostname = true;
      ProtectKernelLogs = true;
      ProtectKernelModules = true;
      ProtectKernelTunables = true;
      ProtectProc = "invisible";
      ProtectSystem = "full";
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
      SystemCallArchitectures = "native";
    };
    systemd.services.plex.after = serviceDeps;
    systemd.services.plex.bindsTo = serviceDeps;

    services.plex = {
      enable = true;
      openFirewall = true;
      package = unstable.plex;
      user = cfg.user.name;
      group = cfg.group.name;
    };
  };
}
