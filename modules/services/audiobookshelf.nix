{
  options,
  config,
  lib,
  utils,
  pkgs,
  inputs,
  globals,
  ...
}:
let
  cfg = config.modules.services.audiobookshelf;
  home-ops = config.repo.secrets.home-ops;
  localPath = "/mnt/mali/${cfg.nfsShare}";
  serviceDeps = [ "${utils.escapeSystemdPath localPath}.mount" ];
in
{
  options.modules.services.audiobookshelf = {
    enable = lib.mkEnableOption "audiobookshelf";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "audiobookshelf.example.com";
      description = "The domain to use for the audiobookshelf";
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
      device = "${lib.my.cidrToIp config.repo.secrets.global.nodes.mali.dataCIDR}:/mnt/${cfg.nfsShare}";
      fsType = "nfs";
    };

    modules.zfs.datasets.properties = {
      "tank/encrypted/svc/audiobookshelf"."mountpoint" = config.services.audiobookshelf.dataDir;
      "tank/encrypted/svc/audiobookshelf"."com.sun:auto-snapshot" = "false";
    };

    services.audiobookshelf = {
      enable = true;
      openFirewall = false;
      user = cfg.user.name;
      group = cfg.group.name;
    };
  };
}
