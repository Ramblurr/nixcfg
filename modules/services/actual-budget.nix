{
  options,
  config,
  lib,
  pkgs,
  inputs,
  unstable,
  ...
}:
let
  cfg = config.modules.services.actual-server;
  home-ops = config.repo.secrets.home-ops;
  httpPort = toString cfg.ports.http;
  stateDirActual = "/var/lib/private/actual-budget";
  stateDirEffective = "/var/lib/actual-budget";
  cfgFile = pkgs.writeText "actual.json" (
    builtins.toJSON {
      dataDir = stateDirEffective;
      hostname = "127.0.0.1";
      port = cfg.ports.http;
      serverFiles = "${stateDirEffective}/server";
      userFiles = "${stateDirEffective}/user";
      loginMethod = "password";
      trustedProxies = [ "127.0.0.1/24" ];
    }
  );
in
{
  options.modules.services.actual-server = {
    enable = lib.mkEnableOption "actual-server";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "budget.example.com";
      description = "The domain to use for the actual-server";
    };
    ports = {
      http = lib.mkOption {
        type = lib.types.port;
        description = "The HTTP port to use for the actual-server";
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
      "rpool/encrypted/safe/svc/actual-budget"."mountpoint" = stateDirActual;
    };
    systemd.services.actual-server = {
      description = "Actual Server";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      preStart = "mkdir -p ${stateDirEffective}/{server,user}";
      documentation = [ "https://actualbudget.org/docs/" ];
      environment = {
        ACTUAL_CONFIG_PATH = "${cfgFile}";
        ACTUAL_UPLOAD_FILE_SYNC_SIZE_LIMIT_MB = "50";
        ACTUAL_UPLOAD_SYNC_ENCRYPTED_FILE_SIZE_LIMIT_MB = "50";
        ACTUAL_UPLOAD_FILE_SIZE_LIMIT_MB = "50";
      };
      unitConfig = {
        RequiresMountsFor = [ stateDirActual ];
      };
      serviceConfig = {
        ExecStart = "${pkgs.my.actual-server}/bin/actual";
        Restart = "always";
        StateDirectory = lib.mkForce (baseNameOf stateDirEffective);
        DynamicUser = true;
        ProtectHome = true;
        ProtectSystem = "strict";
        #MemoryDenyWriteExecute = true;
        #
        #PrivateTmp = true;
        #PrivateDevices = true;
        #ProtectHostname = true;
        #ProtectClock = true;
        #ProtectKernelTunables = true;
        #ProtectKernelModules = true;
        #ProtectKernelLogs = true;
        #ProtectControlGroups = true;
        #NoNewPrivileges = true;
        #RestrictRealtime = true;
        #RestrictSUIDSGID = true;
        #RemoveIPC = true;
        #LockPersonality = true;
        #PrivateMounts = true;
        #PrivateUsers = true;
        #RestrictNamespaces = true;
        #CapabilityBoundingSet = "";
        #SystemCallArchitectures = "native";
        #SystemCallFilter = [ "@system-service" ];
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
      forwardAuth = false;
    };
  };
}
