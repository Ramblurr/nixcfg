{
  config,
  inputs,
  lib,
  ...
}:
let
  cfg = config.modules.services.koreader-sync;
  package = inputs.koreader-syncd.packages.${config.nixpkgs.hostPlatform.system}.default;
  stateDir = "/var/lib/koreader-sync";
in
{
  options.modules.services.koreader-sync = {
    enable = lib.mkEnableOption "koreader-sync";
    domain = lib.mkOption {
      type = lib.types.str;
      description = "The domain to use for koreader-syncd";
    };
    ports = {
      http = lib.mkOption { type = lib.types.port; };
    };
    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption { type = lib.types.unspecified; };
  };
  config = lib.mkIf cfg.enable {
    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/koreader-sync"."mountpoint" = stateDir;
      "rpool/encrypted/safe/svc/koreader-sync"."com.sun:auto-snapshot" = "false";
    };

    systemd.tmpfiles.rules = [
      "d '${stateDir}' 750 ${cfg.user.name} ${cfg.group.name} - -"
      "Z '${stateDir}' 750 ${cfg.user.name} ${cfg.group.name} - -"
    ];
    users.users.${cfg.user.name} = {
      inherit (cfg.user) name;
      inherit (cfg.user) uid;
      isNormalUser = true;
      home = stateDir;
      createHome = false;
      group = lib.mkForce cfg.group.name;
      linger = true;
      # see https://github.com/nikstur/userborn/issues/7
      # autoSubUidGidRange = true;
    };
    users.groups.${cfg.group.name} = {
      inherit (cfg.group) name;
      gid = lib.mkForce cfg.group.gid;
    };
    systemd.services.koreader-syncd = {
      description = "KOReader sync server";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        ExecStart = "${package}/bin/koreader-syncd -a 127.0.0.1:${toString cfg.ports.http} -d ${stateDir}/state.db";
        User = cfg.user.name;
        Group = cfg.group.name;
        Restart = "on-failure";
        RestartSec = "5s";
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ReadWritePaths = stateDir;
        ProtectHome = true;
        PrivateTmp = true;
      };
    };

    modules.services.caddy.routes.koreader = {
      publicHost = cfg.domain;
      upstream = "http://127.0.0.1:${toString cfg.ports.http}";
      requestBodyMaxSize = "10MB";
    };
  };
}
