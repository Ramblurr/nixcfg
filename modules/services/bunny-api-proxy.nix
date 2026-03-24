{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.bunny-api-proxy;

  inherit (lib)
    escapeShellArg
    getExe
    mkEnableOption
    mkIf
    mkOption
    mkPackageOption
    optionalString
    types
    ;

  serviceName = "bunny-api-proxy";
  stateDirActual = "/var/lib/private/${serviceName}";
  stateDirEffective = "/var/lib/${serviceName}";
  databasePath = "${stateDirEffective}/proxy.db";
  startScript = pkgs.writeShellScript "bunny-api-proxy-start" ''
    export BUNNY_API_KEY="$(<"$CREDENTIALS_DIRECTORY/bunny_api_key")"
    export DATABASE_PATH=${escapeShellArg databasePath}
    export LISTEN_ADDR=${escapeShellArg cfg.listenAddress}
    export METRICS_LISTEN_ADDR=${escapeShellArg cfg.metricsListenAddress}
    export LOG_LEVEL=${escapeShellArg cfg.logLevel}
    ${optionalString (cfg.bunnyApiUrl != null) "export BUNNY_API_URL=${escapeShellArg cfg.bunnyApiUrl}"}
    exec ${getExe cfg.package}
  '';
in
{
  options.services.bunny-api-proxy = {
    enable = mkEnableOption "bunny-api-proxy";

    package = mkPackageOption pkgs "bunny-api-proxy" { };

    bunnyApiKeyFile = mkOption {
      type = types.str;
      example = "/run/secrets/bunny-api-key";
      description = "Path to a file containing the Bunny API key.";
    };

    listenAddress = mkOption {
      type = types.str;
      default = "127.0.0.1:8081";
      description = "Address for the main HTTP listener.";
    };

    metricsListenAddress = mkOption {
      type = types.str;
      default = "127.0.0.1:9091";
      description = "Address for the internal metrics listener.";
    };

    logLevel = mkOption {
      type = types.enum [
        "debug"
        "info"
        "warn"
        "error"
      ];
      default = "info";
      description = "Structured log level.";
    };

    bunnyApiUrl = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "https://api.bunny.net";
      description = "Optional Bunny API base URL override.";
    };
  };

  config = mkIf cfg.enable {
    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/${serviceName}"."mountpoint" = stateDirActual;
    };
    systemd.services.${serviceName} = {
      description = "Bunny API Proxy";
      documentation = [ "https://github.com/sipico/bunny-api-proxy" ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart = startScript;
        WorkingDirectory = stateDirEffective;
        Restart = "on-failure";
        RestartSec = "5s";
        DynamicUser = true;
        StateDirectory = serviceName;
        StateDirectoryMode = "0750";
        LoadCredential = [ "bunny_api_key:${cfg.bunnyApiKeyFile}" ];
        UMask = "0077";
        CapabilityBoundingSet = "";
        DeviceAllow = "";
        LockPersonality = true;
        NoNewPrivileges = true;
        PrivateDevices = true;
        PrivateTmp = true;
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
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
          "AF_UNIX"
        ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        SystemCallArchitectures = "native";
        SystemCallFilter = [ "@system-service" ];
      };
    };
  };
}
