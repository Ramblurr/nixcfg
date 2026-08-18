{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.services.soju;
  ircPort = toString cfg.ports.irc;
  stateDirActual = "/var/lib/private/soju";
in
{
  options.modules.services.soju = {
    enable = lib.mkEnableOption "soju";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "soju.example.com";
      description = "The domain to use for soju";
    };
    ports = {
      irc = lib.mkOption {
        type = lib.types.port;
        description = "The irc port to use for soju";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/soju"."mountpoint" = stateDirActual;
    };
    services.soju = {
      enable = true;
      hostName = cfg.domain;
      listen = [ "irc+insecure://:${ircPort}" ];
    };
    networking.firewall.allowedTCPPorts = [ ircPort ];
  };
}
