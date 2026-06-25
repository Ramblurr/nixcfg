{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.jellyplex-watched;

  toEnvValue = value: if lib.isBool value then lib.boolToString value else toString value;

  toEnvironmentFile =
    env:
    lib.generators.toKeyValue {
      mkKeyValue = key: value: "${key}=${lib.escapeShellArg (toEnvValue value)}";
    } env;

  baseEnvironment = {
    DRYRUN = cfg.dryRun;
    DEBUG_LEVEL = cfg.logLevel;
    RUN_ONLY_ONCE = cfg.runOnlyOnce;
    SLEEP_DURATION = cfg.interval;
    LOG_FILE = "log.log";
    MARK_FILE = "mark.log";
    USER_MAPPING = builtins.toJSON cfg.mappings.users;
    LIBRARY_MAPPING = builtins.toJSON cfg.mappings.libraries;
  }
  // lib.optionalAttrs (cfg.plex.urls != [ ]) {
    PLEX_BASEURL = lib.concatStringsSep ", " cfg.plex.urls;
  }
  // lib.optionalAttrs (cfg.jellyfin.urls != [ ]) {
    JELLYFIN_BASEURL = lib.concatStringsSep ", " cfg.jellyfin.urls;
  }
  // lib.optionalAttrs (cfg.emby.urls != [ ]) {
    EMBY_BASEURL = lib.concatStringsSep ", " cfg.emby.urls;
  };

  configEnvironmentFile = pkgs.writeText "jellyplex-watched-env" (
    toEnvironmentFile (baseEnvironment // cfg.extraEnvironment)
  );
in
{
  options.modules.services.jellyplex-watched = {
    enable = lib.mkEnableOption "JellyPlex-Watched";

    package = lib.mkPackageOption pkgs "jellyplex-watched" { };

    environmentFile = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "/run/secrets/jellyplex-watched/env";
      description = ''
        Optional systemd EnvironmentFile containing secrets or overrides, such
        as PLEX_TOKEN and JELLYFIN_TOKEN. Keep API tokens here instead of in
        Nix options so they do not enter the Nix store.
      '';
    };

    dryRun = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Log changes without marking shows or movies as played.";
    };

    runOnlyOnce = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Run one sync pass and exit.";
    };

    logLevel = lib.mkOption {
      type = lib.types.enum [
        "INFO"
        "DEBUG"
        "TRACE"
        "info"
        "debug"
        "trace"
      ];
      default = "INFO";
      apply = lib.toUpper;
      description = "JellyPlex-Watched log level.";
    };

    interval = lib.mkOption {
      type = lib.types.ints.positive;
      default = 3600;
      description = "Seconds between sync passes.";
    };

    mappings = {
      users = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        default = { };
        description = "Map usernames that differ between media servers.";
      };

      libraries = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        default = { };
        description = "Map library names that differ between media servers.";
      };
    };

    plex.urls = lib.mkOption {
      type = lib.types.listOf (lib.types.strMatching "^https?://.*$");
      default = [ ];
      description = "Plex base URLs.";
    };

    jellyfin.urls = lib.mkOption {
      type = lib.types.listOf (lib.types.strMatching "^https?://.*$");
      default = [ ];
      description = "Jellyfin base URLs.";
    };

    emby.urls = lib.mkOption {
      type = lib.types.listOf (lib.types.strMatching "^https?://.*$");
      default = [ ];
      description = "Emby base URLs.";
    };

    extraEnvironment = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.oneOf [
          lib.types.bool
          lib.types.int
          lib.types.str
        ]
      );
      default = { };
      example = {
        SYNC_FROM_PLEX_TO_JELLYFIN = true;
        MAX_THREADS = 2;
      };
      description = ''
        Extra non-secret environment variables for JellyPlex-Watched. Do not
        put API tokens here; use environmentFile for secrets.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.jellyplex-watched = {
      description = "Sync watched status between Jellyfin, Plex, and Emby locally";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];

      serviceConfig = {
        Type = "exec";
        ExecStart = lib.getExe cfg.package;
        EnvironmentFile = [
          configEnvironmentFile
        ]
        ++ lib.optional (cfg.environmentFile != null) cfg.environmentFile;
        DynamicUser = true;
        StateDirectory = "jellyplex-watched";
        WorkingDirectory = "/var/lib/jellyplex-watched";
        Restart = "on-failure";
        RestartSec = "30s";
        UMask = "0077";
        CapabilityBoundingSet = "";
        LockPersonality = true;
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHome = true;
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectSystem = "strict";
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
