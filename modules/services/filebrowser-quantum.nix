{
  config,
  lib,
  pkgs,
  utils,
  ...
}:
let
  cfg = config.modules.services.filebrowser-quantum;
  stateDir = "/var/lib/filebrowser-quantum";
  cacheDir = "/var/cache/filebrowser-quantum";
  upstream = "http://127.0.0.1:${toString cfg.ports.http}";
  settingsFormat = pkgs.formats.yaml { };
  sourcePaths = lib.unique (map (source: source.path) cfg.sources);
  settingsFile = settingsFormat.generate "filebrowser-quantum-config.yaml" (
    lib.recursiveUpdate {
      server = {
        port = cfg.ports.http;
        listen = "127.0.0.1";
        database = "${stateDir}/database.db";
        cacheDir = cacheDir;
        disableUpdateCheck = true;
        sources = map (source: {
          inherit (source) name path;
          config.defaultEnabled = true;
        }) cfg.sources;
      };
      auth = {
        adminUsername = cfg.adminUsername;
        methods = {
          password.enabled = false;
          proxy = {
            enabled = true;
            createUser = true;
            header = cfg.authProxyHeader;
          };
        };
      };
      integrations.media.ffmpegPath = "${pkgs.ffmpeg-headless}/bin";
      userDefaults.permissions = {
        api = false;
        admin = false;
        modify = true;
        share = false;
        realtime = false;
        delete = true;
        create = true;
        download = true;
      };
    } cfg.settings
  );
in
{
  options.modules.services.filebrowser-quantum = {
    enable = lib.mkEnableOption "FileBrowser Quantum";

    package = lib.mkPackageOption pkgs "filebrowser-quantum" { };

    domain = lib.mkOption {
      type = lib.types.str;
      example = "files.example.com";
      description = "The domain to use for FileBrowser Quantum.";
    };

    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
    };

    adminUsername = lib.mkOption {
      type = lib.types.str;
      default = "casey";
      description = "Proxy-authenticated username that should be promoted to admin.";
    };

    authProxyHeader = lib.mkOption {
      type = lib.types.str;
      default = "X-authentik-username";
      description = "Header FileBrowser Quantum should trust for proxy authentication.";
    };

    ports = {
      http = lib.mkOption { type = lib.types.port; };
    };

    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption { type = lib.types.unspecified; };

    sources = lib.mkOption {
      type = lib.types.listOf (
        lib.types.submodule {
          options = {
            name = lib.mkOption {
              type = lib.types.str;
              description = "Display name for the source.";
            };
            path = lib.mkOption {
              type = lib.types.str;
              description = "Filesystem path to expose.";
            };
          };
        }
      );
      default = [ ];
      description = "Filesystem paths exposed by FileBrowser Quantum.";
    };

    settings = lib.mkOption {
      type = settingsFormat.type;
      default = { };
      description = "Extra FileBrowser Quantum settings merged into the generated YAML config.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.sources != [ ];
        message = "modules.services.filebrowser-quantum.sources must contain at least one source.";
      }
    ];

    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/filebrowser-quantum"."mountpoint" = stateDir;
      "rpool/encrypted/safe/svc/filebrowser-quantum"."com.sun:auto-snapshot" = "false";
    };

    systemd.services.filebrowser-quantum = {
      description = "FileBrowser Quantum";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      unitConfig = {
        RequiresMountsFor = sourcePaths ++ [
          stateDir
          cacheDir
        ];
      };
      serviceConfig = {
        ExecStart = utils.escapeSystemdExecArgs [
          (lib.getExe cfg.package)
          "-c"
          settingsFile
        ];
        StateDirectory = "filebrowser-quantum";
        StateDirectoryMode = "0750";
        CacheDirectory = "filebrowser-quantum";
        CacheDirectoryMode = "0750";
        WorkingDirectory = stateDir;
        Restart = "on-failure";
        RestartSec = "5s";
        User = cfg.user.name;
        Group = cfg.group.name;
        UMask = "0077";
        ReadWritePaths = [
          stateDir
          cacheDir
        ]
        ++ sourcePaths;
        CapabilityBoundingSet = "";
        AmbientCapabilities = "";
        LockPersonality = true;
        NoNewPrivileges = true;
        DevicePolicy = "closed";
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
        RestrictAddressFamilies = [
          "AF_UNIX"
          "AF_INET"
          "AF_INET6"
        ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        MemoryDenyWriteExecute = true;
      };
    };

    modules.services.ingress.virtualHosts.${cfg.domain} = {
      acmeHost = cfg.ingress.domain;
      inherit upstream;
      forwardAuth = cfg.ingress.forwardAuth;
      extraConfig = ''
        client_max_body_size 0;
      '';
    };

    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [ cfg.domain ];
      };
    };
  };
}
