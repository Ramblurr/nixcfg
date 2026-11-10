{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.neogrok;
  cacheDir = "/var/cache/neogrok";
  indexer = pkgs.writeShellApplication {
    name = "neogrok-index-mirrors";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.findutils
      pkgs.git
      pkgs.zoekt
    ];
    text = builtins.readFile ../../pkgs/neogrok/index-mirrors.sh;
  };
  sandbox = {
    User = "neogrok";
    Group = "neogrok";
    NoNewPrivileges = true;
    PrivateTmp = true;
    ProtectSystem = "strict";
    ProtectHome = true;
    RestrictAddressFamilies = [
      "AF_UNIX"
      "AF_INET"
      "AF_INET6"
    ];
    UMask = "0027";
  };
in
{
  options.modules.services.neogrok = {
    enable = lib.mkEnableOption "Neogrok search over dependency mirrors";
    listenAddress = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
    };
    port = lib.mkOption {
      type = lib.types.port;
      default = 3081;
    };
    zoektPort = lib.mkOption {
      type = lib.types.port;
      default = 6070;
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.modules.services.dependency-mirror.enable;
        message = "Neogrok requires the dependency mirror service.";
      }
    ];
    users.users.neogrok = {
      isSystemUser = true;
      group = "neogrok";
    };
    users.groups.neogrok = { };
    systemd.tmpfiles.rules = [
      "d ${cacheDir} 0750 neogrok neogrok -"
      "d ${cacheDir}/index 0750 neogrok neogrok -"
      "d ${cacheDir}/repos 0750 neogrok neogrok -"
    ];
    systemd.services.neogrok = {
      description = "Neogrok code search frontend";
      wantedBy = [ "multi-user.target" ];
      wants = [
        "zoekt.service"
        "network-online.target"
      ];
      after = [
        "network-online.target"
        "zoekt.service"
      ];
      environment = {
        HOST = cfg.listenAddress;
        PORT = toString cfg.port;
        ORIGIN = "http://${cfg.listenAddress}:${toString cfg.port}";
        ZOEKT_URL = "http://127.0.0.1:${toString cfg.zoektPort}";
        NODE_ENV = "production";
      };
      serviceConfig = sandbox // {
        ExecStart = lib.getExe pkgs.neogrok;
        Restart = "on-failure";
        RestartSec = 5;
        MemoryMax = "1G";
      };
    };
    systemd.services.zoekt = {
      description = "Zoekt search backend for Neogrok";
      wantedBy = [ "multi-user.target" ];
      after = [ "systemd-tmpfiles-setup.service" ];
      environment.GOMAXPROCS = "2";
      serviceConfig = sandbox // {
        ExecStart = "${pkgs.zoekt}/bin/zoekt-webserver -listen 127.0.0.1:${toString cfg.zoektPort} -index ${cacheDir}/index -rpc -html=false";
        Restart = "on-failure";
        MemoryMax = "4G";
      };
    };
    systemd.services.neogrok-index = {
      description = "Index retained bare dependency mirrors with Universal Ctags";
      after = [ "systemd-tmpfiles-setup.service" ];
      unitConfig.RequiresMountsFor = [ "/var/lib/dependency-mirror" ];
      environment = {
        CTAGS_COMMAND = "${pkgs.universal-ctags}/bin/ctags";
        GOMAXPROCS = "2";
      };
      serviceConfig = sandbox // {
        Type = "oneshot";
        SupplementaryGroups = [ "dependency-mirror" ];
        ExecStart = "${lib.getExe indexer} /var/lib/dependency-mirror/repos ${cacheDir} https://${config.modules.services.dependency-mirror.domain}";
        ReadWritePaths = [ cacheDir ];
        MemoryMax = "6G";
        CPUQuota = "200%";
        Nice = 10;
        TimeoutStartSec = "4h";
      };
    };
    systemd.timers.neogrok-index = {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnBootSec = "2m";
        OnUnitInactiveSec = "5m";
      };
    };
  };
}
