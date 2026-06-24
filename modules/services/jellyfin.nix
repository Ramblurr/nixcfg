{
  config,
  lib,
  utils,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.jellyfin;
  localPath = "/mnt/mali/${cfg.nfsShare}";
  serviceDeps = [ "${utils.escapeSystemdPath localPath}.mount" ];
in
{
  options.modules.services.jellyfin = {
    enable = lib.mkEnableOption "jellyfin";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "jelly.example.com";
      description = "The domain to use for Jellyfin";
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
    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [ cfg.domain ];
      };
    };

    modules.services.ingress.virtualHosts.${cfg.domain} = {
      acmeHost = cfg.ingress.domain;
      upstream = "http://127.0.0.1:8096";
      forwardAuth = cfg.ingress.forwardAuth;
      upstreamExtraConfig = ''
        proxy_buffering off;
      '';
    };

    users.users.${cfg.user.name} = {
      inherit (cfg.user) name;
      uid = lib.mkForce cfg.user.uid;
      isSystemUser = true;
      group = lib.mkForce cfg.group.name;
      extraGroups = lib.unique (
        (cfg.user.extraGroups or [ ])
        ++ [
          "media"
          "render"
          "video"
        ]
      );
    };

    users.groups.${cfg.group.name} = {
      inherit (cfg.group) name;
      gid = lib.mkForce cfg.group.gid;
    };

    fileSystems."${localPath}" = {
      device = "${lib.my.cidrToIp config.repo.secrets.global.nodes.mali.dataCIDR}:/mnt/${cfg.nfsShare}";
      fsType = "nfs";
    };

    modules.zfs.datasets.properties = {
      "tank/encrypted/svc/jellyfin"."mountpoint" = config.services.jellyfin.dataDir;
      "tank/encrypted/svc/jellyfin"."com.sun:auto-snapshot" = "false";
    };

    systemd.services.jellyfin.after = serviceDeps;
    systemd.services.jellyfin.bindsTo = serviceDeps;
    systemd.services.jellyfin.unitConfig.RequiresMountsFor = [ localPath ];
    systemd.services.jellyfin.serviceConfig = {
      SupplementaryGroups = [
        "media"
        "render"
        "video"
      ];
      DeviceAllow = [
        "char-drm rw"
        "/dev/dri rwm"
      ];
      PrivateUsers = lib.mkForce false;
    };

    services.jellyfin = {
      enable = true;
      openFirewall = true;
      package = pkgs.jellyfin;
      user = cfg.user.name;
      group = cfg.group.name;
      hardwareAcceleration.enable = true;
      hardwareAcceleration.type = "qsv";
      hardwareAcceleration.device = "/dev/dri/renderD128";
      transcoding.enableHardwareEncoding = true;
    };
  };
}
