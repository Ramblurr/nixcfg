{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.dependency-mirror;
  stateDir = "/var/lib/dependency-mirror";
  dataset = "rpool/encrypted/safe/svc/dependency-mirror";
  package = pkgs.callPackage ../../pkgs/dependency-mirror.nix { };
  serviceConfig = {
    User = "dependency-mirror";
    Group = "dependency-mirror";
    UMask = "0027";
    WorkingDirectory = stateDir;
    NoNewPrivileges = true;
    PrivateTmp = true;
    ProtectSystem = "strict";
    ProtectHome = true;
    ReadWritePaths = [ stateDir ];
    RestrictAddressFamilies = [
      "AF_UNIX"
      "AF_INET"
      "AF_INET6"
    ];
  };
in
{
  options.modules.services.dependency-mirror = {
    enable = lib.mkEnableOption "Gickup dependency mirrors with an EDN editor and cgit";
    domain = lib.mkOption {
      type = lib.types.str;
      description = "Private ingress hostname for cgit and the editor";
    };
    listenAddress = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
      description = "Loopback address used by the editor and cgit backends";
    };
    port = lib.mkOption {
      type = lib.types.port;
      default = 8084;
    };
    cgitPort = lib.mkOption {
      type = lib.types.port;
      default = 8085;
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.services.tailscale.enable;
        message = "The dependency mirror requires Tailscale for authenticated network access.";
      }
      {
        assertion = cfg.port != cfg.cgitPort;
        message = "The dependency mirror editor and cgit need distinct ports.";
      }
    ];
    users.users.dependency-mirror = {
      isSystemUser = true;
      group = "dependency-mirror";
      home = stateDir;
    };
    users.groups.dependency-mirror = { };
    environment.systemPackages = [ package ];
    modules.zfs.datasets = {
      properties.${dataset}.mountpoint = stateDir;
      services.${dataset} = [
        "dependency-mirror"
        "dependency-mirror-sync"
        "nginx"
      ];
    };
    systemd.tmpfiles.rules = [
      "d ${stateDir} 0750 dependency-mirror dependency-mirror -"
      "d ${stateDir}/repos 0750 dependency-mirror dependency-mirror -"
    ];
    systemd.services.dependency-mirror = {
      description = "Dependency mirror inventory editor";
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [
        "tailscaled.service"
        "network-online.target"
      ];
      unitConfig.RequiresMountsFor = [ stateDir ];
      serviceConfig = serviceConfig // {
        ExecStart = "${lib.getExe package} serve --state-dir ${stateDir} --bind ${cfg.listenAddress} --port ${toString cfg.port} --cgit-url https://${cfg.domain}/ --base-path /edit --public-origin https://${cfg.domain}";
        Restart = "on-failure";
        RestartSec = 10;
      };
    };
    systemd.services.dependency-mirror-sync = {
      description = "Update dependency mirrors";
      wants = [ "network-online.target" ];
      after = [
        "network-online.target"
        "dependency-mirror.service"
      ];
      unitConfig.RequiresMountsFor = [ stateDir ];
      serviceConfig = serviceConfig // {
        Type = "oneshot";
        ExecStart = "${lib.getExe package} sync --state-dir ${stateDir}";
        TimeoutStartSec = "12h";
      };
    };
    systemd.timers.dependency-mirror-sync = {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "04:30";
        RandomizedDelaySec = "15m";
        Persistent = true;
      };
    };
    modules.services.caddy.routes.dependency-mirror = {
      publicHost = cfg.domain;
      handlerConfig = ''
        @editor path /edit /edit/*
        handle @editor {
          reverse_proxy ${cfg.listenAddress}:${toString cfg.port}
        }
        handle {
          reverse_proxy ${cfg.listenAddress}:${toString cfg.cgitPort}
        }
      '';
    };
    services.cgit.dependency-mirror = {
      enable = true;
      group = "dependency-mirror";
      scanPath = "${stateDir}/repos";
      gitHttpBackend.checkExportOkFiles = false;
      settings = {
        root-title = "Dependency mirrors";
        root-desc = "Read-only vendor and dependency repositories";
        enable-http-clone = true;
        cache-scanrc-ttl = 0;
        cache-root-ttl = 0;
        section-from-path = 2;
        enable-index-links = true;
      };
    };
    services.nginx.virtualHosts.dependency-mirror = {
      listen = [
        {
          addr = cfg.listenAddress;
          port = cfg.cgitPort;
        }
      ];
      locations."~ /.+/(info/refs|git-upload-pack)".fastcgiParams = {
        # The reader and writer deliberately have different UIDs.
        GIT_CONFIG_COUNT = "2";
        GIT_CONFIG_KEY_0 = "safe.directory";
        GIT_CONFIG_VALUE_0 = "${stateDir}/repos/*";
        GIT_CONFIG_KEY_1 = "http.receivepack";
        GIT_CONFIG_VALUE_1 = "false";
      };
    };
  };
}
