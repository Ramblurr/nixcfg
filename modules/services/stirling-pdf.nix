{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.services.stirling-pdf;
in
{
  options.modules.services.stirling-pdf = {
    enable = lib.mkEnableOption "stirling-pdf";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "pdf.example.com";
      description = "The domain to use for stirling-pdf";
    };
    ports = {
      http = lib.mkOption {
        type = lib.types.port;
        description = "The HTTP port to use for stirling-pdf";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    services.stirling-pdf = {
      enable = true;
      environment = {
        SERVER_PORT = cfg.ports.http;
        SYSTEM_DEFAULTLOCALE = "en-US";
        INSTALL_BOOK_AND_ADVANCED_HTML_OPS = "true";
      };
    };
    site.gatus.endpoints = [
      {
        name = "Stirling PDF";
        group = config.site.gatus.groups.home;
        url = "https://${cfg.domain}/";
      }
    ];

    modules.services.caddy.routes.pdf = {
      publicHost = cfg.domain;
      upstream = "http://127.0.0.1:${toString cfg.ports.http}";
      requestBodyMaxSize = "10MB";
    };
  };
}
