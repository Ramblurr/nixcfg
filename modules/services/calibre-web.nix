{
  config,
  lib,
  pkgs,
  utils,
  ...
}:

let
  cfg = config.modules.services.calibre-web;
  home-ops = config.repo.secrets.home-ops;
  mediaLocalPath = "/mnt/mali/${cfg.mediaNfsShare}";
in
{
  options.modules.services.calibre-web = {
    enable = lib.mkEnableOption "calibre-web";
    domain = lib.mkOption {
      type = lib.types.str;
      description = "The domain to use for the calibre-web";
    };

    domainKobo = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "The domain to use for the kobo endpoint";
    };
    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
    };
    ports = {
      http = lib.mkOption { type = lib.types.port; };
    };
    mediaNfsShare = lib.mkOption { type = lib.types.str; };
    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption { type = lib.types.unspecified; };
  };
  config = lib.mkIf cfg.enable {
    users.users.${cfg.user.name} = lib.mkForce {
      name = cfg.user.name;
      uid = lib.mkForce cfg.user.uid;
      isSystemUser = true;
      group = lib.mkForce cfg.group.name;
      extraGroups = lib.mkForce [ "media" ];
    };

    users.groups.${cfg.group.name} = {
      name = cfg.group.name;
      gid = lib.mkForce cfg.group.gid;
    };

    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/calibre-web"."mountpoint" = "/var/lib/calibre-web";
    };

    systemd.services.calibre-web.unitConfig = {
      RequiresMountsFor = [
        mediaLocalPath
        "/var/lib/calibre-web"
      ];
    };
    systemd.services.calibre-web.serviceConfig = {
      SupplementaryGroups = [ "media" ];
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
      CapabilityBoundingSet = "";
      AmbientCapabilities = "";
      MemoryDenyWriteExecute = true;
      UMask = "0002";
      RestrictAddressFamilies = [
        "AF_UNIX"
        "AF_INET"
        "AF_INET6"
        "AF_NETLINK"
      ];
      ReadWritePaths = [ "${mediaLocalPath}/books" ];
    };

    systemd.services.calibre-web.serviceConfig = {
      CacheDirectory = "calibre-web";
      Environment = [ "CACHE_DIR=/var/cache/calibre-web" ];
    };
    services.calibre-web = {
      enable = true;
      listen.port = cfg.ports.http;
      listen.ip = "127.0.0.1";
      options = {
        calibreLibrary = "${mediaLocalPath}/books";
        enableBookConversion = true;
        enableBookUploading = true;
        enableKepubify = true;
        reverseProxyAuth = {
          enable = true;
          header = "X-authentik-username";
        };
      };
    };

    modules.services.ingress.virtualHosts.${cfg.domain} = {
      acmeHost = cfg.ingress.domain;
      upstream = "http://127.0.0.1:${toString cfg.ports.http}";
      forwardAuth = true;
      extraConfig = ''
        client_max_body_size 0;
      '';
    };

    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [
          cfg.domain
          cfg.domainKobo
        ];
      };
    };
    modules.services.ingress.virtualHosts.${cfg.domainKobo} = lib.mkIf (cfg.domainKobo != "") {
      acmeHost = cfg.ingress.domain;
      upstream = "http://127.0.0.1:${toString cfg.ports.http}/";
      upstreamExtraConfig = ''
        proxy_buffering on;
        proxy_buffers 4 256k;
        proxy_buffer_size 256k;
        proxy_busy_buffers_size 256k;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header X-Scheme https;
      '';
      extraConfig = ''
        client_max_body_size 0;
      '';
    };
  };
}
