{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.radicle-seed;
  service = "radicle-seed";
  dataDir = "/var/lib/${service}";
  homeDir = "/home/${cfg.user.name}";
in
#inherit (inputs.heartwood.packages.${pkgs.stdenv.system}) radicle-cli radicle-node;
{
  options.modules.services.radicle-seed = {
    enable = lib.mkEnableOption "radicle seed node";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "rad.example.com";
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
    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption { type = lib.types.unspecified; };
  };
  config = lib.mkIf cfg.enable {
    systemd.services.radicle-node = {
      enable = true;
      after = [
        "syslog.target"
        "network.target"
      ];
      wantedBy = [ "default.target" ];
      serviceConfig = {
        #User = cfg.user;
        #WorkingDirectory = cfg.home;
        KillMode = "process";
        Restart = "always";
        RestartSec = "3";
        Environment = [
          "RAD_HOME=${cfg.home}/.radicle"
          "RUST_BACKTRACE=1"
          "RUST_LOG=info"
        ];

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

      script = ''
        PATH="${pkgs.git}/bin:$PATH"
        ${radicle-node}/bin/radicle-node \
          --listen 127.0.0.1:${toString cfg.ports.http} \
          --force
      '';
    };

    ###############
    users.users.${cfg.user.name} = {
      inherit (cfg.user) name;
      uid = lib.mkForce cfg.user.uid;
      isNormalUser = true;
      group = lib.mkForce cfg.group.name;
      home = homeDir;
      linger = true;
      createHome = false;
      autoSubUidGidRange = true;
    };

    users.groups.${cfg.group.name} = {
      inherit (cfg.group) name;
      gid = lib.mkForce cfg.group.gid;
    };

    modules.zfs.datasets.properties = {
      "tank/svc/${service}"."mountpoint" = dataDir;
    };

    systemd.tmpfiles.rules = [
      "z '${dataDir}' 750 ${cfg.user.name} ${cfg.group.name} - -"
      "d ${homeDir} 750 ${cfg.user.name} ${cfg.group.name} - -"
    ];

    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [ cfg.domain ];
      };
    };
    modules.services.ingress.virtualHosts.${cfg.domain} = {
      acmeHost = cfg.ingress.domain;
      upstream = "http://127.0.0.1:${toString cfg.ports.http}";
      forwardAuth = true;
    };

  };
}
