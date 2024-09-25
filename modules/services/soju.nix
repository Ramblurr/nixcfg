{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.services.soju;
  home-ops = config.repo.secrets.home-ops;
  ircPort = toString cfg.ports.irc;
  stateDirActual = "/var/lib/private/soju";
  stateDirEffective = "/var/lib/soju";
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
    #ingress = lib.mkOption {
    #  type = lib.types.submodule (
    #    lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
    #  );
    #};
  };

  config = lib.mkIf cfg.enable {
    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/soju"."mountpoint" = stateDirActual;
    };
    #modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
    #  "${cfg.ingress.domain}" = {
    #    externalDomains = [ cfg.domain ];
    #  };
    #};
    #modules.services.ingress.virtualHosts.${cfg.domain} = {
    #  acmeHost = cfg.ingress.domain;
    #  upstream = "http://127.0.0.1:${httpPort}";
    #  forwardAuth = false;
    #};
    services.soju = {
      enable = true;
      hostName = cfg.domain;
      listen = [ "irc+insecure://:${ircPort}" ];
    };
    networking.firewall.allowedTCPPorts = [ ircPort ];
  };
}
