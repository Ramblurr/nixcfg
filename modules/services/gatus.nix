{
  config,
  lib,
  ...
}:

let
  cfg = config.modules.services.gatus;
  stateDirActual = "/var/lib/private/gatus";
  stateDirEffective = "/var/lib/gatus";
in
{
  options.modules.services.gatus = {
    enable = lib.mkEnableOption "Gatus";
    domain = lib.mkOption {
      type = lib.types.nonEmptyStr;
      example = "status.example.com";
      description = "Public hostname for Gatus";
    };
    port = lib.mkOption {
      type = lib.types.port;
      default = 10022;
      description = "Local HTTP port for Gatus";
    };
    allowedRemoteIPs = lib.mkOption {
      type = lib.types.listOf lib.types.nonEmptyStr;
      default = [ ];
      description = "Peer addresses and CIDRs allowed through the Caddy route";
    };
  };

  config = lib.mkIf cfg.enable {
    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/gatus".mountpoint = stateDirActual;
    };

    services.gatus = {
      enable = true;
      settings = {
        web.port = cfg.port;
        storage = {
          type = "sqlite";
          path = "${stateDirEffective}/data.db";
        };
      };
    };

    systemd.services.gatus.unitConfig.RequiresMountsFor = [ stateDirActual ];

    modules.services.caddy.routes.gatus = {
      publicHost = cfg.domain;
      upstream = "http://127.0.0.1:${toString cfg.port}";
      inherit (cfg) allowedRemoteIPs;
    };
  };
}
