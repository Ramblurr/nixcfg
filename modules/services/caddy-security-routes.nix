{ config, lib, ... }:
let
  cfg = config.modules.services.caddy;
  normalizeRoute =
    route:
    {
      aliases = [ ];
      allowedRemoteIPs = [ ];
      upstream = null;
      root = null;
      handlerConfig = null;
      errorHandlerConfig = null;
      webSockets = true;
      http3 = true;
      directWan = false;
      requestBodyMaxSize = null;
      requestHeaders = { };
      responseHeaders = { };
      dialTimeout = null;
      flushInterval = null;
      staticResponses = { };
    }
    // route;
  normalizedRoutes = lib.mapAttrs (_: normalizeRoute) cfg.legacyRoutes;
in
{
  options.modules.services.caddy = {
    routes = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "Normalized route registry for the legacy caddy-security module";
    };
    protectedRoutes = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "Ignored protected-route registry retained for globally imported legacy route owners";
    };
    legacyRoutes = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "Route registry retained only by hosts awaiting caddy-security migration";
    };
  };

  config.modules.services.caddy.routes = lib.mkIf (cfg.legacyRoutes != { }) normalizedRoutes;
}
