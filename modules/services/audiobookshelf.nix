{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.services.audiobookshelf;
  localPath = "/mnt/mali/${cfg.nfsShare}";
  stateDir = "/var/lib/${config.services.audiobookshelf.dataDir}";
  mediaLocalPath = "/mnt/mali/${cfg.nfsShare}";
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
    ports = {
      http = lib.mkOption { type = lib.types.port; };
    };
    nfsShare = lib.mkOption { type = lib.types.str; };
    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption { type = lib.types.unspecified; };
  };
  config = lib.mkIf cfg.enable {
    users.users.${cfg.user.name} = {
      inherit (cfg.user) name;
      uid = lib.mkForce cfg.user.uid;
      isSystemUser = true;
      group = lib.mkForce cfg.group.name;
      extraGroups = [ "media" ];
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
      "tank/encrypted/svc/audiobookshelf"."mountpoint" = stateDir;
      "tank/encrypted/svc/audiobookshelf"."com.sun:auto-snapshot" = "false";
    };

    systemd.tmpfiles.rules = [
      "z '${stateDir}' 750 ${cfg.user.name} ${cfg.user.group} - -"
    ];

    systemd.services.audiobookshelf.unitConfig = {
      RequiresMountsFor = [
        mediaLocalPath
        stateDir
      ];
    };
    systemd.services.audiobookshelf.serviceConfig = {
      SupplementaryGroups = [ "media" ];
      UMask = "0002";
      ReadWritePaths = [ "${mediaLocalPath}/misc/Audiobooks" ];
      LockPersonality = true;
      NoNewPrivileges = true;
      PrivateDevices = true;
      PrivateMounts = true;
      PrivateTmp = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      ProtectControlGroups = true;
      ProtectClock = true;
      ProtectProc = "invisible";
      ProtectHostname = true;
      ProtectKernelLogs = true;
      ProtectKernelModules = true;
      ProtectKernelTunables = true;
      ProcSubset = "pid";
      RemoveIPC = true;
      RestrictNamespaces = true;
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
      #SystemCallArchitectures = "native";
      #SystemCallFilter = [
      #  "@system-service"
      #  "~@privileged"
      #  "~@resources"
      #];
      #CapabilityBoundingSet = "";
      #AmbientCapabilities = "";
      #MemoryDenyWriteExecute = true;
      #RestrictAddressFamilies = [
      #  "AF_UNIX"
      #  "AF_INET"
      #  "AF_INET6"
      #  "AF_NETLINK"
      #];
    };
    services.audiobookshelf = {
      enable = true;
      openFirewall = false;
      port = cfg.ports.http;
      user = cfg.user.name;
      group = cfg.group.name;
    };

    modules.services.ingress.virtualHosts.${cfg.domain} = {
      acmeHost = cfg.ingress.domain;
      upstream = "http://127.0.0.1:${toString cfg.ports.http}";
      extraConfig = ''
        client_max_body_size 0;
      '';
    };

    #modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
    #  "${cfg.ingress.domain}" = {
    #    externalDomains = [
    #      cfg.domain
    #    ];
    #  };
    #};
  };
}
