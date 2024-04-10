{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.services.archivebox;

  httpPort = toString cfg.ports.http;
in
{
  options.modules.services.archivebox = {
    enable = lib.mkEnableOption "archivebox";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "archive.example.com";
      description = "The domain to use";
    };
    ports = {
      http = lib.mkOption {
        type = lib.types.port;
        description = "The HTTP port to use";
      };
    };
    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
    };
  };
  config = lib.mkIf cfg.enable {
    modules.zfs.datasets.properties = {
      "tank/svc/archivebox"."mountpoint" = "/var/lib/private/archivebox";
    };

    systemd.services.archivebox = {
      unitConfig = {
        RequiresMountsFor = [ "/var/lib/private/archivebox" ];
      };
      environment = {
        REVERSE_PROXY_USER_HEADER = "X-authentik-username";
        REVERSE_PROXY_WHITELIST = "127.0.0.1/24";
        PUBLIC_ADD_VIEW = "True";
      };
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.archivebox}/bin/archivebox server --quick-init 127.0.0.1:${httpPort}";
        ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
        ExecStop = "${pkgs.coreutils}/bin/kill -s QUIT $MAINPID";
        Restart = "always";
        RestartSec = "2";
        StateDirectory = "archivebox";
        WorkingDirectory = "/var/lib/archivebox";
        UMask = 77;
        DynamicUser = true;
        ProtectHome = true;
        ProtectSystem = "strict";
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectHostname = true;
        ProtectClock = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectKernelLogs = true;
        ProtectControlGroups = true;
        NoNewPrivileges = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        RemoveIPC = true;
        LockPersonality = true;
        PrivateMounts = true;
        PrivateUsers = true;
        RestrictNamespaces = true;
        CapabilityBoundingSet = "";
        SystemCallArchitectures = "native";
        SystemCallFilter = [ "@system-service" ];
        MemoryDenyWriteExecute = true;
      };
    };

    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [ cfg.domain ];
      };
    };
    modules.services.ingress.virtualHosts.${cfg.domain} = {
      acmeHost = cfg.ingress.domain;
      upstream = "http://127.0.0.1:${httpPort}";
      forwardAuth = true;
      extraConfig = ''
        client_max_body_size 0;
        client_header_buffer_size 64k;
      '';
    };
  };
}
