{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.caddy-security;
  plainRoutes = config.modules.services.caddy.routes;

  appType = lib.types.submodule (
    { name, ... }:
    {
      options = {
        publicHost = lib.mkOption {
          type = lib.types.nonEmptyStr;
          description = "Public hostname routed to this application by nginx";
        };

        upstream = lib.mkOption {
          type = lib.types.nonEmptyStr;
          description = "Caddy reverse-proxy upstream";
        };

        portalPath = lib.mkOption {
          type = lib.types.nonEmptyStr;
          default = "/auth";
          description = "Application-local authentication portal path";
        };

        oidc = {
          issuerURL = lib.mkOption {
            type = lib.types.nonEmptyStr;
            description = "Pocket ID OIDC issuer URL";
          };

          clientID = lib.mkOption {
            type = lib.types.nonEmptyStr;
            description = "Application-specific Pocket ID client ID";
          };

          clientSecretEnv = lib.mkOption {
            type = lib.types.nonEmptyStr;
            description = "Environment variable containing the Pocket ID client secret";
          };

          realm = lib.mkOption {
            type = lib.types.nonEmptyStr;
            default = "${name}-pocket-id";
            description = "Application-specific caddy-security OAuth realm";
          };
        };

        signingKeyEnv = lib.mkOption {
          type = lib.types.nonEmptyStr;
          description = "Environment variable containing the portal signing key";
        };

        cookiePrefix = lib.mkOption {
          type = lib.types.nonEmptyStr;
          default = lib.toUpper (lib.replaceStrings [ "-" ] [ "_" ] name);
          description = "Prefix for application-specific caddy-security cookies";
        };

        requiredGroups = lib.mkOption {
          type = lib.types.listOf lib.types.nonEmptyStr;
          description = "Pocket ID groups allowed to use the application";
        };

        bypassPathPrefixes = lib.mkOption {
          type = lib.types.listOf lib.types.nonEmptyStr;
          default = [ ];
          description = "Path prefixes proxied without caddy-security authorization";
        };

        identityHeaders = lib.mkOption {
          type = lib.types.attrsOf lib.types.nonEmptyStr;
          default = { };
          description = "Upstream identity headers mapped to caddy-security claims";
        };
      };
    }
  );

  applications = lib.attrValues cfg.applications;
  appNames = builtins.attrNames cfg.applications;
  appId = name: lib.replaceStrings [ "-" ] [ "_" ] name;
  applicationIds = map appId appNames;
  providerName = name: "${appId name}_pocket_id";
  portalName = name: "${appId name}_portal";
  policyName = name: "${appId name}_policy";
  allUnique = values: builtins.length values == builtins.length (lib.unique values);
  validEnvName = value: builtins.match "[A-Z][A-Z0-9_]*" value != null;
  issuerURL = app: lib.removeSuffix "/" app.oidc.issuerURL;
  portalPath = app: lib.removeSuffix "/" app.portalPath;

  mkProvider = name: app: ''
    oauth identity provider ${providerName name} {
      realm ${app.oidc.realm}
      driver generic
      base_auth_url ${issuerURL app}
      metadata_url ${issuerURL app}/.well-known/openid-configuration
      client_id ${app.oidc.clientID}
      client_secret {env.${app.oidc.clientSecretEnv}}
      scopes openid email profile groups
      extract preferred_username groups from userinfo
      retry_attempts 5
      retry_interval 2
    }
  '';

  mkPortal = name: app: ''
    authentication portal ${portalName name} {
      crypto default token lifetime 3600
      crypto key sign-verify {env.${app.signingKeyEnv}}
      enable identity provider ${providerName name}
      cookie strip domain
      cookie lifetime 3600
      cookie samesite lax
      cookie insecure off
      set cookie name prefix ${app.cookiePrefix}
      trust login redirect uri domain exact ${app.publicHost} path prefix /
      trust logout redirect uri domain exact ${app.publicHost} path prefix /
    }
  '';

  mkAclRule = group: ''
    acl rule {
      match role ${builtins.toJSON group}
      allow stop log info
    }
  '';

  mkHeaderInjection = header: claim: ''
    inject header ${header} from ${builtins.toJSON claim}
  '';

  mkPolicy = name: app: ''
    authorization policy ${policyName name} {
      crypto key verify {env.${app.signingKeyEnv}}
      set auth url ${portalPath app}/oauth2/${app.oidc.realm}
      set session_id cookie name ${app.cookiePrefix}_SESSION_ID
      set access_token cookie name ${app.cookiePrefix}_ACCESS_TOKEN
      ${lib.concatMapStringsSep "\n" mkAclRule app.requiredGroups}
      acl default deny
      ${lib.concatStringsSep "\n" (lib.mapAttrsToList mkHeaderInjection app.identityHeaders)}
    }
  '';

  defaultIdentityHeaders = [
    "Remote-User"
    "Remote-Name"
    "Remote-Email"
    "Remote-Groups"
    "X-Auth-Request-*"
    "X-authentik-*"
    "X_authentik_*"
  ];

  mkHeaderScrub =
    app:
    lib.concatMapStringsSep "\n" (header: "request_header -${header}") (
      lib.unique (defaultIdentityHeaders ++ builtins.attrNames app.identityHeaders)
    );

  mkBypass = app: pathPrefix: ''
    handle ${pathPrefix}* {
      route {
        ${mkHeaderScrub app}
        reverse_proxy ${app.upstream}
      }
    }
  '';

  mkApplicationRoute = name: app: ''
    @app_${appId name} host ${app.publicHost}
    handle @app_${appId name} {
      handle ${portalPath app}* {
        authenticate with ${portalName name}
      }
      ${lib.concatMapStringsSep "\n" (mkBypass app) app.bypassPathPrefixes}
      handle {
        route {
          ${mkHeaderScrub app}
          authorize with ${policyName name}
          reverse_proxy ${app.upstream}
        }
      }
    }
  '';

  quote = value: builtins.toJSON value;
  plainId = name: lib.replaceStrings [ "-" ] [ "_" ] name;
  responseId = path: builtins.substring 0 12 (builtins.hashString "sha256" path);

  mkPlainStaticResponse =
    name: path: response:
    let
      matcher = "@plain_${plainId name}_${responseId path}";
      headers = lib.concatStringsSep "\n" (
        lib.mapAttrsToList (header: value: "header ${matcher} ${header} ${quote value}") response.headers
      );
    in
    ''
      ${matcher} path ${path}
      ${headers}
      respond ${matcher} ${quote response.body} ${toString response.status}
    '';

  mkPlainProxy =
    route:
    let
      headers = lib.concatStringsSep "\n" (
        lib.mapAttrsToList (header: value: "header_up ${header} ${quote value}") route.requestHeaders
      );
      transport = lib.optionalString (route.dialTimeout != null) ''
        transport http {
          dial_timeout ${route.dialTimeout}
        }
      '';
    in
    ''
      reverse_proxy ${route.upstream} {
        ${headers}
        ${lib.optionalString (route.flushInterval != null) "flush_interval ${route.flushInterval}"}
        ${transport}
      }
    '';

  mkPlainRoute =
    name: route:
    let
      responseHeaders = lib.concatStringsSep "\n" (
        lib.mapAttrsToList (header: value: "header ${header} ${quote value}") route.responseHeaders
      );
      staticResponses = lib.concatStringsSep "\n" (
        lib.mapAttrsToList (mkPlainStaticResponse name) route.staticResponses
      );
    in
    ''
      @plain_${plainId name} host ${route.publicHost}
      handle @plain_${plainId name} {
        ${lib.optionalString route.webSockets "# WebSocket upgrades are handled by Caddy reverse_proxy."}
        ${responseHeaders}
        ${staticResponses}
        ${lib.optionalString (route.requestBodyMaxSize != null) ''
          request_body {
            max_size ${route.requestBodyMaxSize}
          }
        ''}
        ${
          if route.upstream != null then
            mkPlainProxy route
          else
            ''
              root * ${route.root}
              file_server
            ''
        }
      }
    '';

  protectedPublicHosts = map (app: app.publicHost) applications;
  plainPublicHosts = map (route: route.publicHost) (builtins.attrValues plainRoutes);
  publicHosts = protectedPublicHosts ++ plainPublicHosts;
  clientIDs = map (app: app.oidc.clientID) applications;
  realmNames = map (app: app.oidc.realm) applications;
  cookiePrefixes = map (app: app.cookiePrefix) applications;
  secretEnvNames = lib.concatMap (app: [
    app.oidc.clientSecretEnv
    app.signingKeyEnv
  ]) applications;
in
{
  options.modules.services.caddy-security = {
    enable = lib.mkEnableOption "loopback caddy-security authentication behind nginx";

    listenAddress = lib.mkOption {
      type = lib.types.enum [
        "127.0.0.1"
        "::1"
      ];
      default = "127.0.0.1";
      description = "Loopback address for the internal Caddy listener";
    };

    listenPort = lib.mkOption {
      type = lib.types.port;
      default = 18080;
      description = "Port for the internal Caddy listener";
    };

    environmentFile = lib.mkOption {
      type = lib.types.nonEmptyStr;
      description = "Runtime path to the SOPS-rendered Caddy environment file";
    };

    applications = lib.mkOption {
      type = lib.types.attrsOf appType;
      default = { };
      description = "Applications protected by distinct Pocket ID clients";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.applications != { };
        message = "caddy-security requires at least one application";
      }
      {
        assertion =
          lib.hasPrefix "/" cfg.environmentFile && !lib.hasPrefix "/nix/store/" cfg.environmentFile;
        message = "caddy-security environmentFile must be an absolute runtime path outside the Nix store";
      }
      {
        assertion = allUnique publicHosts;
        message = "caddy-security application publicHost values must be unique";
      }
      {
        assertion = allUnique clientIDs;
        message = "caddy-security applications must use distinct Pocket ID client IDs";
      }
      {
        assertion = allUnique realmNames;
        message = "caddy-security application OIDC realms must be unique";
      }
      {
        assertion = allUnique cookiePrefixes;
        message = "caddy-security application cookie prefixes must be unique";
      }
      {
        assertion = allUnique secretEnvNames;
        message = "caddy-security secret environment variable names must be unique";
      }
      {
        assertion = lib.all (name: builtins.match "[A-Za-z0-9_-]+" name != null) appNames;
        message = "caddy-security application names may contain only letters, digits, underscores, and hyphens";
      }
      {
        assertion = allUnique applicationIds;
        message = "caddy-security application names must remain unique after Caddy identifier normalization";
      }
      {
        assertion = lib.all (app: lib.hasPrefix "https://" app.oidc.issuerURL) applications;
        message = "caddy-security OIDC issuer URLs must use HTTPS";
      }
      {
        assertion = lib.all (app: lib.hasPrefix "/" app.portalPath && app.portalPath != "/") applications;
        message = "caddy-security portal paths must begin with a slash and may not be the site root";
      }
      {
        assertion = lib.all (app: builtins.match "[A-Za-z0-9._-]+" app.oidc.realm != null) applications;
        message = "caddy-security OIDC realms contain an unsupported character";
      }
      {
        assertion = lib.all (app: validEnvName app.cookiePrefix) applications;
        message = "caddy-security cookie prefixes must use uppercase cookie syntax";
      }
      {
        assertion = lib.all (
          app: lib.all (pathPrefix: lib.hasPrefix "/" pathPrefix) app.bypassPathPrefixes
        ) applications;
        message = "caddy-security bypass path prefixes must begin with a slash";
      }
      {
        assertion = lib.all (app: app.requiredGroups != [ ]) applications;
        message = "caddy-security applications must require at least one Pocket ID group";
      }
      {
        assertion = lib.all (
          app: validEnvName app.oidc.clientSecretEnv && validEnvName app.signingKeyEnv
        ) applications;
        message = "caddy-security secret environment variable names must use uppercase shell syntax";
      }
      {
        assertion = lib.all (
          app:
          lib.all (header: builtins.match "[A-Za-z0-9_-]+" header != null) (
            builtins.attrNames app.identityHeaders
          )
        ) applications;
        message = "caddy-security identity header names contain an unsupported character";
      }
    ];

    services.caddy = {
      enable = true;
      package = pkgs.caddy-with-security;
      inherit (cfg) environmentFile;
      openFirewall = false;
      globalConfig = ''
        auto_https off
        admin 127.0.0.1:2019
        servers {
          trusted_proxies static 127.0.0.1/32 ::1/128
          trusted_proxies_strict
          client_ip_headers X-Forwarded-For
        }
        security {
          ${lib.concatStringsSep "\n" (lib.mapAttrsToList mkProvider cfg.applications)}
          ${lib.concatStringsSep "\n" (lib.mapAttrsToList mkPortal cfg.applications)}
          ${lib.concatStringsSep "\n" (lib.mapAttrsToList mkPolicy cfg.applications)}
        }
      '';
      extraConfig = ''
        http://:${toString cfg.listenPort} {
          bind ${cfg.listenAddress}
          route {
            @health {
              host caddy-health.invalid
              path /healthz
            }
            respond @health "ok" 200
            @unknown_host not host ${lib.concatStringsSep " " publicHosts}
            respond @unknown_host 421
            ${lib.concatStringsSep "\n" (lib.mapAttrsToList mkPlainRoute plainRoutes)}
            ${lib.concatStringsSep "\n" (lib.mapAttrsToList mkApplicationRoute cfg.applications)}
          }
        }
      '';
    };

    systemd.services.caddy = {
      requires = [ "sops-install-secrets.service" ];
      after = [ "sops-install-secrets.service" ];
      serviceConfig = {
        CapabilityBoundingSet = "";
        LockPersonality = true;
        MemoryDenyWriteExecute = true;
        PrivateTmp = true;
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectSystem = "strict";
        RestrictAddressFamilies = [
          "AF_UNIX"
          "AF_INET"
          "AF_INET6"
          "AF_NETLINK"
        ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
      };
    };
  };
}
