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
      "rpool/encrypted/safe/svc/plex"."mountpoint" = config.services.plex.dataDir;
      "rpool/encrypted/safe/svc/plex"."com.sun:auto-snapshot" = "false";
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
