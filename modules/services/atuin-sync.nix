{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.services.atuin-sync;
in
{
  options.modules.services.atuin-sync = {
    enable = lib.mkEnableOption "atuin-sync";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "atuin.example.com";
      description = "The domain to use for the atuin-sync";
    };
    ports = {
      http = lib.mkOption {
        type = lib.types.port;
        description = "The HTTP port to use for the atuin-sync";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    services.atuin = {
      enable = true;
      openRegistration = false;
      port = cfg.ports.http;
    };
    site.gatus.endpoints = [
      {
        name = "Atuin Sync";
        group = "Home & Personal";
        url = "https://${cfg.domain}/";
      }
    ];

    modules.services.caddy.routes.atuin = {
      publicHost = cfg.domain;
      upstream = "http://127.0.0.1:${toString cfg.ports.http}";
      requestBodyMaxSize = "10MB";
    };
  };
}
