{
  config,
  lib,
  pkgs,
  unstable,
  utils,
  ...
}:

let
  cfg = config.modules.services.calibre-web;
  home-ops = config.repo.secrets.home-ops;
  mediaLocalPath = "/mnt/mali/${cfg.mediaNfsShare}";
  serviceDeps = [ "${utils.escapeSystemdPath mediaLocalPath}.mount" ];
in
{
  options.modules.services.calibre-web = {
    enable = lib.mkEnableOption "calibre-web";
    domain = lib.mkOption {
      type = lib.types.str;
      description = "The domain to use for the calibre-web";
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
    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [ cfg.domain ];
      };
    };
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

    systemd.services.calibre-web.serviceConfig = {
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
      DeviceAllow = "";
      MemoryDenyWriteExecute = true;
      UMask = "0077";
      RestrictAddressFamilies = [
        "AF_UNIX"
        "AF_INET"
        "AF_INET6"
        "AF_NETLINK"
      ];
      ReadWritePaths = [ "${mediaLocalPath}/books" ];
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
      };
    };

    services.nginx.virtualHosts.${cfg.domain} = {
      useACMEHost = cfg.ingress.domain;
      forceSSL = true;
      kTLS = true;
      extraConfig = ''
        client_max_body_size 0;
        client_header_buffer_size 64k;
      '';
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString cfg.ports.http}";
        recommendedProxySettings = true;
      };
    };
  };
}
