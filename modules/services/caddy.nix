{ config, lib, ... }:
let
  staticResponseType = lib.types.submodule {
    options = {
      body = lib.mkOption {
        type = lib.types.lines;
        description = "Static response body";
      };

      status = lib.mkOption {
        type = lib.types.ints.between 100 599;
        default = 200;
        description = "Static response HTTP status";
      };

      headers = lib.mkOption {
        type = lib.types.attrsOf lib.types.nonEmptyStr;
        default = { };
        description = "Response headers added to this static response";
      };
    };
  };

  routeType = lib.types.submodule {
    options = {
      publicHost = lib.mkOption {
        type = lib.types.nonEmptyStr;
        description = "Public hostname passed from nginx to loopback Caddy";
      };

      upstream = lib.mkOption {
        type = lib.types.nullOr lib.types.nonEmptyStr;
        default = null;
        description = "Caddy reverse-proxy upstream, including unix// socket addresses";
      };

      root = lib.mkOption {
        type = lib.types.nullOr lib.types.nonEmptyStr;
        default = null;
        description = "Absolute static file root served by Caddy";
      };

      webSockets = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Require Caddy reverse_proxy WebSocket upgrade support";
      };

      requestBodyMaxSize = lib.mkOption {
        type = lib.types.nullOr lib.types.nonEmptyStr;
        default = null;
        description = "Maximum request body size accepted by Caddy; null leaves it unlimited";
      };

      requestHeaders = lib.mkOption {
        type = lib.types.attrsOf lib.types.nonEmptyStr;
        default = { };
        description = "Request headers explicitly set on the upstream proxy request";
      };

      responseHeaders = lib.mkOption {
        type = lib.types.attrsOf lib.types.nonEmptyStr;
        default = { };
        description = "Response headers added for the route";
      };

      dialTimeout = lib.mkOption {
        type = lib.types.nullOr lib.types.nonEmptyStr;
        default = null;
        description = "Caddy HTTP transport dial timeout";
      };

      flushInterval = lib.mkOption {
        type = lib.types.nullOr lib.types.nonEmptyStr;
        default = null;
        description = "Caddy reverse-proxy flush interval for streaming responses";
      };

      staticResponses = lib.mkOption {
        type = lib.types.attrsOf staticResponseType;
        default = { };
        description = "Exact-path static responses served before the proxy";
      };
    };
  };
  cfg = config.modules.services.caddy;
  routeNames = builtins.attrNames cfg.routes;
  routeIds = map (name: lib.replaceStrings [ "-" ] [ "_" ] name) routeNames;
  routes = builtins.attrValues cfg.routes;
  allUnique = values: builtins.length values == builtins.length (lib.unique values);
  validHeader = header: builtins.match "[A-Za-z0-9_-]+" header != null;
  isRouteTarget = route: (route.upstream != null) != (route.root != null);
  hasAbsoluteRoot = route: route.root == null || lib.hasPrefix "/" route.root;
  hasAbsoluteResponsePaths =
    route: lib.all (path: lib.hasPrefix "/" path) (builtins.attrNames route.staticResponses);
  hasValidHeaders =
    route:
    lib.all validHeader (builtins.attrNames route.requestHeaders)
    && lib.all validHeader (builtins.attrNames route.responseHeaders)
    && lib.all (response: lib.all validHeader (builtins.attrNames response.headers)) (
      builtins.attrValues route.staticResponses
    );
in
{
  options.modules.services.caddy.routes = lib.mkOption {
    type = lib.types.attrsOf routeType;
    default = { };
    description = "Plain loopback Caddy routes that do not use caddy-security";
  };

  config.assertions = [
    {
      assertion = lib.all isRouteTarget routes;
      message = "Each plain Caddy route requires exactly one of upstream or root";
    }
    {
      assertion = lib.all hasAbsoluteRoot routes;
      message = "Plain Caddy static roots must be absolute paths";
    }
    {
      assertion = lib.all hasAbsoluteResponsePaths routes;
      message = "Plain Caddy static response paths must begin with a slash";
    }
    {
      assertion = allUnique routeIds;
      message = "Plain Caddy route names must remain unique after Caddy identifier normalization";
    }
    {
      assertion = lib.all hasValidHeaders routes;
      message = "Plain Caddy header names contain an unsupported character";
    }
  ];
}
