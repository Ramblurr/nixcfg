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
    domain1 = lib.mkOption {
      type = lib.types.str;
      example = "auth.example.com";
      description = "The domain to use for the authentik";
    };

    domain2 = lib.mkOption {
      type = lib.types.str;
      example = "auth.example.com";
      description = "The domain to use for the authentik";
    };
    ingress1 = lib.mkOption { type = lib.types.str; };
    ingress2 = lib.mkOption { type = lib.types.str; };

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
    modules.services.ingress.domains = {
      "${cfg.ingress1}" = {
        externalDomains = [ cfg.domain1 ];
      };
      "${cfg.ingress2}" = {
        externalDomains = [ cfg.domain2 ];
      };
    };
    sops.secrets."authentik/env" = {
      sopsFile = ../../configs/home-ops/shared.sops.yml;
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

    services.nginx.virtualHosts = {

      ${cfg.domain1} = {
        useACMEHost = cfg.ingress1;
        forceSSL = true;
        kTLS = true;
        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString cfg.ports.http}";
          recommendedProxySettings = true;
          proxyWebsockets = true;
        };
      };
      ${cfg.domain2} = {
        useACMEHost = cfg.ingress2;
        forceSSL = true;
        kTLS = true;
        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString cfg.ports.http}";
          recommendedProxySettings = true;
          proxyWebsockets = true;
        };
      };
    };
  };
}
