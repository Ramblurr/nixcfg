{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.immich-machine-learning;
in
{
  # The nixpkgs Immich module requires the full server to enable machine learning.
  # It also adds PostgreSQL setup commands when its database is disabled.
  # This module runs machine learning without those services or commands.
  options.modules.services.immich-machine-learning = {
    enable = lib.mkEnableOption "standalone Immich machine learning";
    package = lib.mkPackageOption pkgs "immich-machine-learning" { };
    host = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
      description = "Listen address. Expose only to trusted Immich clients; the ML API has no authentication.";
    };
    port = lib.mkOption {
      type = lib.types.port;
      default = 3003;
      description = "ML HTTP port; this module does not open the firewall.";
    };
    gpu = lib.mkEnableOption "NVIDIA device access for a CUDA-capable ML package";
    environment = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      description = "Additional Immich ML environment variables.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !config.services.immich.enable;
        message = "Standalone Immich ML cannot share its unit and identity with the full Immich service.";
      }
    ];
    users.users.immich = {
      isSystemUser = true;
      group = "immich";
      extraGroups = lib.optionals cfg.gpu [
        "video"
        "render"
      ];
    };
    users.groups.immich = { };

    systemd.services.immich-machine-learning = {
      description = "Standalone Immich machine learning";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      environment = {
        IMMICH_HOST = cfg.host;
        IMMICH_PORT = toString cfg.port;
        MACHINE_LEARNING_CACHE_FOLDER = "/var/cache/immich";
        XDG_CACHE_HOME = "/var/cache/immich";
        MPLCONFIGDIR = "/var/cache/immich";
        # Gunicorn's control socket belongs in a private systemd runtime directory,
        # not the system user's non-writable home directory.
        XDG_RUNTIME_DIR = "/run/immich-machine-learning";
        MACHINE_LEARNING_WORKERS = "1";
        MACHINE_LEARNING_REQUEST_THREADS = "1";
        MACHINE_LEARNING_MODEL_INTER_OP_THREADS = "1";
        MACHINE_LEARNING_MODEL_INTRA_OP_THREADS = "1";
        MACHINE_LEARNING_WORKER_TIMEOUT = "300";
      }
      // lib.optionalAttrs cfg.gpu {
        LD_LIBRARY_PATH = "/run/opengl-driver/lib";
      }
      // cfg.environment;
      serviceConfig = {
        ExecStart = lib.getExe cfg.package;
        User = "immich";
        Group = "immich";
        CacheDirectory = "immich";
        CacheDirectoryMode = "0700";
        RuntimeDirectory = "immich-machine-learning";
        RuntimeDirectoryMode = "0700";
        Restart = "on-failure";
        RestartSec = 3;
        UMask = "0077";
        Nice = 10;
        CPUWeight = 20;
        IOWeight = 20;
        CapabilityBoundingSet = "";
        NoNewPrivileges = true;
        PrivateUsers = true;
        PrivateTmp = true;
        PrivateDevices = !cfg.gpu;
        PrivateMounts = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
          "AF_UNIX"
        ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
      };
    };
  };
}
