{
  options,
  config,
  lib,
  pkgs,
  inputs,
  mine,
  ...
}:
let
  cfg = config.modules.services.authentik;
  home-ops = config.repo.secrets.home-ops;
in
{
  options.modules.services.authentik = {
    enable = lib.mkEnableOption "authentik";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "auth.example.com";
      description = "The domain to use for the authentik";
    };

    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
    };
    ports = {
      http = lib.mkOption {
        type = lib.types.port;
        description = "The HTTP port to use for authentik";
      };

      https = lib.mkOption {
        type = lib.types.port;
        description = "The HTTP port to use for authentik";
      };
    };
  };
  config = lib.mkIf cfg.enable {
    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [ cfg.domain ];
      };
    };
    sops.secrets."authentik/env" = {
      owner = config.services.authentik.user;
      group = config.services.authentik.group;
      mode = "400";
    };
    services.authentik = {
      enable = true;
      package = mine.authentik;
      logLevel = "info";
      listen.address = "127.0.0.1";
      listen.http = cfg.ports.http;
      listen.https = cfg.ports.https;
      environmentFile = config.sops.secrets."authentik/env".path;
    };

    services.nginx.virtualHosts.${cfg.domain} = {
      useACMEHost = cfg.ingress.domain;
      forceSSL = true;
      kTLS = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString cfg.ports.http}";
        recommendedProxySettings = true;
      };
    };
  };
}
