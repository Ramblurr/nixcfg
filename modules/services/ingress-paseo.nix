{
  config,
  lib,
  ...
}:

let
  cfg = config.modules.services.ingress-paseo;
  homeDomain = config.repo.secrets.global.domain.home;
in
{
  options.modules.services.ingress-paseo.enable = lib.mkEnableOption "Paseo ingress";

  config = lib.mkIf cfg.enable {
    modules.services.caddy.routes.paseo = {
      publicHost = "paseo.${homeDomain}";
      upstream = "http://quine.prim.${homeDomain}:6767";
      requestBodyMaxSize = "100MB";
      requestHeaders = {
        Host = "{http.request.host}";
        X-Forwarded-For = "{http.request.header.X-Forwarded-For}";
        X-Forwarded-Proto = "https";
      };
      dialTimeout = "120s";
      flushInterval = "-1";
    };
  };
}
