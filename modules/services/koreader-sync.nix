{
  config,
  inputs,
  lib,
  ...
}:
let
  cfg = config.modules.services.koreader-sync;
  package = inputs.koreader-syncd.packages.${config.nixpkgs.hostPlatform.system}.default;
  stateDirActual = "/var/lib/private/koreader-syncd";
  stateDirEffective = "/var/lib/koreader-syncd";
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
  };
  config = lib.mkIf cfg.enable {
    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/koreader-syncd"."mountpoint" = stateDirActual;
    };
    systemd.services.koreader-syncd = {
      description = "KOReader sync server";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      unitConfig.RequiresMountsFor = [ stateDirActual ];

      serviceConfig = {
        ExecStart = "${package}/bin/koreader-syncd -a 127.0.0.1:${toString cfg.ports.http} -d ${stateDirEffective}/state.db";
        DynamicUser = true;
        StateDirectory = baseNameOf stateDirEffective;
        Restart = "on-failure";
        RestartSec = "5s";
        NoNewPrivileges = true;
        ProtectSystem = "strict";
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
