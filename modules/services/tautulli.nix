{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.tautulli;
in
{
  options.modules.services.tautulli = {
    enable = lib.mkEnableOption "tautulli";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "tautulli.example.com";
      description = "The domain to use for the tautulli";
    };
    ports = {
      http = lib.mkOption {
        type = lib.types.port;
        description = "The HTTP port to use for tautulli";
      };
    };
    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption {
      type = lib.types.str;
      default = "nogroup";
    };
  };
  config = lib.mkIf cfg.enable {
    users.users.${cfg.user.name} = {
      inherit (cfg.user) name;
      uid = lib.mkForce cfg.user.uid;
      isSystemUser = true;
      group = lib.mkForce cfg.group;
    };

    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/tautulli"."mountpoint" = config.services.tautulli.dataDir;
      "rpool/encrypted/safe/svc/tautulli"."com.sun:auto-snapshot" = "false";
    };

    modules.services.caddy.protectedRoutes.tautulli = {
      publicHost = cfg.domain;
      upstream = "http://127.0.0.1:${toString cfg.ports.http}";
    };

    services.tautulli = {
      enable = true;
      dataDir = "/var/lib/tautulli";
      openFirewall = false;
      package = pkgs.tautulli;
      port = cfg.ports.http;
      user = cfg.user.name;
      inherit (cfg) group;
    };

    systemd.services.tautulli.serviceConfig = {
      UMask = 77;
      DeviceAllow = "";
      LockPersonality = true;
      NoNewPrivileges = true;
      PrivateDevices = true;
      PrivateTmp = true;
      PrivateUsers = true;
      ProcSubset = "pid";
      ProtectClock = true;
      ProtectControlGroups = true;
      ProtectHome = true;
      ProtectHostname = true;
      ProtectKernelLogs = true;
      ProtectKernelModules = true;
      ProtectKernelTunables = true;
      ProtectProc = "invisible";
      ProtectSystem = "strict";
      RemoveIPC = true;
      RestrictNamespaces = true;
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
      SystemCallArchitectures = "native";
      SystemCallFilter = [
        "@system-service"
        "~@resources"
        "~@privileged"
      ];
    };
  };
}
