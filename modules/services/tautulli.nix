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
  cfg = config.modules.services.tautulli;
  home-ops = config.repo.secrets.home-ops;
in
{
  options.modules.services.tautulli = {
    enable = lib.mkEnableOption "tautulli";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "tautulli.example.com";
      description = "The domain to use for the tautulli";
    };
    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
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
    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [ cfg.domain ];
      };
    };
    users.users.${cfg.user.name} = {
      name = cfg.user.name;
      uid = lib.mkForce cfg.user.uid;
      isSystemUser = true;
      group = lib.mkForce cfg.group;
    };

    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/tautulli"."mountpoint" = config.services.tautulli.dataDir;
      "rpool/encrypted/safe/svc/tautulli"."com.sun:auto-snapshot" = "false";
    };

    services.tautulli = {
      enable = true;
      dataDir = "/var/lib/tautulli";
      openFirewall = false;
      package = unstable.tautulli;
      port = cfg.ports.http;
      user = cfg.user.name;
      group = cfg.group;
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
