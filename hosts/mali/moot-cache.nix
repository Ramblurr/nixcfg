{
  config,
  lib,
  pkgs,
  unstable,
  ...
}:
let
  authKeyCredential = "ts-authkey";
  stateDir = "/var/lib/moot-cache";
  ncps = config.services.ncps.server.addr;
  readOnlyPort = 3401;
  inherit (import ../../config/moot-nix-cache.nix) nodePort;
  start = pkgs.writeShellScript "moot-cache-start" ''
    set -eu
    TS_AUTHKEY=$(< "$CREDENTIALS_DIRECTORY/${authKeyCredential}")
    export TS_AUTHKEY
    exec ${
      lib.escapeShellArgs [
        (lib.getExe unstable.ts-plug)
        "-hostname"
        "moot-cache"
        "-advertise-tags"
        "tag:moot-cache"
        "-dir"
        stateDir
        "-https-port"
        "443:127.0.0.1:${toString readOnlyPort}"
        "-port"
        (toString nodePort)
        "-response-header-timeout"
        "0"
      ]
    }
  '';
in
{
  networking.firewall.allowedUDPPorts = [ nodePort ];

  services.caddy.virtualHosts."http://:${toString readOnlyPort}" = {
    listenAddresses = [ "127.0.0.1" ];
    logFormat = null;
    extraConfig = ''
      @substitute {
        method GET HEAD
        path /nix-cache-info /*.narinfo /nar/*
      }
      handle @substitute {
        reverse_proxy ${ncps}
      }
      handle {
        respond 403
      }
    '';
  };

  modules.services.onepassword-systemd-credentials.consumers.moot-cache.${authKeyCredential} =
    "op://home-ops-prod/moot-cache/authkey";

  users.users.moot-cache = {
    isSystemUser = true;
    group = "moot-cache";
  };
  users.groups.moot-cache = { };
  environment.persistence."/persist".directories = [ stateDir ];

  systemd.services.moot-cache = {
    description = "ncps on its own tailnet node for the Moot VMs";
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [
      "network-online.target"
      "ncps.service"
    ];
    startLimitIntervalSec = 0;
    serviceConfig = {
      ExecStart = start;
      Restart = "always";
      RestartSec = 5;

      User = "moot-cache";
      Group = "moot-cache";
      StateDirectory = "moot-cache";
      StateDirectoryMode = "0700";
      UMask = "0077";

      CapabilityBoundingSet = "";
      NoNewPrivileges = true;
      RestrictAddressFamilies = [
        "AF_INET"
        "AF_INET6"
        "AF_UNIX"
        "AF_NETLINK"
      ];
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      PrivateDevices = true;
      ProtectKernelTunables = true;
      ProtectKernelModules = true;
      ProtectKernelLogs = true;
      ProtectControlGroups = true;
      ProtectClock = true;
      ProtectHostname = true;
      ProtectProc = "invisible";
      RestrictNamespaces = true;
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
      LockPersonality = true;
      MemoryDenyWriteExecute = true;
      SystemCallArchitectures = "native";
      SystemCallFilter = [
        "@system-service"
        "~@privileged"
      ];
      SystemCallErrorNumber = "EPERM";
    };
  };
}
