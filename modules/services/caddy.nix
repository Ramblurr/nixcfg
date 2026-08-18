{
  config,
  lib,
  pkgs,
  ...
}:
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
        description = "Primary public hostname routed by Caddy";
      };
      aliases = lib.mkOption {
        type = lib.types.listOf lib.types.nonEmptyStr;
        default = [ ];
        description = "Additional public hostnames handled by the same route";
      };
      allowedRemoteIPs = lib.mkOption {
        type = lib.types.listOf lib.types.nonEmptyStr;
        default = [ ];
        description = "Peer IP addresses or CIDRs admitted to this route";
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
      handlerConfig = lib.mkOption {
        type = lib.types.nullOr lib.types.lines;
        default = null;
        description = "Caddyfile handlers for routes that need more than one proxy or static-file handler";
      };
      errorHandlerConfig = lib.mkOption {
        type = lib.types.nullOr lib.types.lines;
        default = null;
        description = "Caddyfile error handlers for this route";
      };
      webSockets = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Require Caddy reverse_proxy WebSocket upgrade support";
      };
      http3 = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Allow HTTP/3 for this route on the public Caddy listener";
      };
      directWan = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Also expose this route on the dedicated direct-WAN Caddy listener";
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

  protectedRouteType = lib.types.submodule (
    { name, ... }: {
      options = {
        publicHost = lib.mkOption {
          type = lib.types.nonEmptyStr;
          description = "Public hostname routed to this protected application by Caddy";
        };
        aliases = lib.mkOption {
          type = lib.types.listOf lib.types.nonEmptyStr;
          default = [ ];
          description = "Additional public hostnames handled by the protected route";
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
        clientID = lib.mkOption {
          type = lib.types.nullOr lib.types.nonEmptyStr;
          default = null;
          description = "Pocket ID client ID; defaults to the registration name";
        };
        oidcRealm = lib.mkOption {
          type = lib.types.nonEmptyStr;
          default = "${name}-pocket-id";
          description = "Pocket ID OIDC realm used for this protected route";
        };
        requiredGroups = lib.mkOption {
          type = lib.types.listOf lib.types.nonEmptyStr;
          default = [ ];
          description = "Pocket ID groups allowed to use the application; empty uses the shared default";
        };
        bypassPathPrefixes = lib.mkOption {
          type = lib.types.listOf lib.types.nonEmptyStr;
          default = [ ];
          description = "Path prefixes proxied without Caddy authentication";
        };
        healthCheckPath = lib.mkOption {
          type = lib.types.nullOr lib.types.nonEmptyStr;
          default = null;
          description = "Upstream URI served at the exact unauthenticated /_health/gatus path";
        };
        identityHeaders = lib.mkOption {
          type = lib.types.attrsOf lib.types.nonEmptyStr;
          default = { };
          description = "Upstream identity headers mapped to caddy-security claims";
        };
        http3 = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Allow HTTP/3 for this protected route on the public Caddy listener";
        };
      };
    }
  );

  cfg = config.modules.services.caddy;
  onepassword = config.modules.services.onepassword-systemd-credentials;
  plainRoutes = cfg.routes;
  inherit (cfg) protectedRoutes;
  routeHosts = route: [ route.publicHost ] ++ route.aliases;
  routeNames = builtins.attrNames plainRoutes;
  protectedNames = builtins.attrNames protectedRoutes;
  routes = builtins.attrValues plainRoutes;
  applications = builtins.attrValues protectedRoutes;
  hasRoutes = plainRoutes != { } || protectedRoutes != { };
  edgeConfigured = cfg.edge.certificateDomains != [ ] || cfg.edge.certificateHosts != [ ];
  caddyEnabled = hasRoutes;
  hasApplications = protectedRoutes != { };
  allUnique = values: builtins.length values == builtins.length (lib.unique values);
  validHeader = header: builtins.match "[A-Za-z0-9_-]+" header != null;
  isRouteTarget =
    route:
    builtins.length (
      lib.filter (target: target != null) [
        route.upstream
        route.root
        route.handlerConfig
      ]
    ) == 1;
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
  appId = name: lib.replaceStrings [ "-" ] [ "_" ] name;
  providerName = name: "${appId name}_pocket_id";
  portalName = name: "${appId name}_portal";
  policyName = name: "${appId name}_policy";
  secretPrefix = name: lib.toUpper (lib.replaceStrings [ "-" ] [ "_" ] name);
  clientSecretEnv = name: "${secretPrefix name}_OIDC_CLIENT_SECRET";
  signingKeyEnv = name: "${secretPrefix name}_SIGNING_KEY";
  effectiveClientID = name: app: if app.clientID == null then name else app.clientID;
  effectiveGroups =
    app: if app.requiredGroups == [ ] then cfg.auth.requiredGroups else app.requiredGroups;
  issuerURL = lib.removeSuffix "/" (if cfg.auth.issuerURL == null then "" else cfg.auth.issuerURL);
  acmeEmail = if cfg.edge.acmeEmail == null then "invalid@example.invalid" else cfg.edge.acmeEmail;
  portalPath = app: lib.removeSuffix "/" app.portalPath;
  directWanListenAddress =
    if cfg.edge.directWan.listenAddress == null then "127.0.0.1" else cfg.edge.directWan.listenAddress;

  mkProvider = name: app: ''
    oauth identity provider ${providerName name} {
      realm ${app.oidcRealm}
      driver generic
      base_auth_url ${issuerURL}
      metadata_url ${issuerURL}/.well-known/openid-configuration
      client_id ${effectiveClientID name app}
      client_secret {env.${clientSecretEnv name}}
      scopes openid email profile groups
      extract preferred_username groups from userinfo
      retry_attempts 5
      retry_interval 2
    }
  '';

  mkPortal = name: app: ''
    authentication portal ${portalName name} {
      crypto default token lifetime 3600
      crypto key sign-verify {env.${signingKeyEnv name}}
      enable identity provider ${providerName name}
      cookie strip domain
      cookie lifetime 3600
      cookie samesite lax
      cookie insecure off
      set cookie name prefix ${secretPrefix name}
      ${lib.concatMapStringsSep "\n" (
        host: "trust login redirect uri domain exact ${host} path prefix /"
      ) (routeHosts app)}
      ${lib.concatMapStringsSep "\n" (
        host: "trust logout redirect uri domain exact ${host} path prefix /"
      ) (routeHosts app)}
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
      crypto key verify {env.${signingKeyEnv name}}
      set auth url ${portalPath app}/oauth2/${app.oidcRealm}
      set session_id cookie name ${secretPrefix name}_SESSION_ID
      set access_token cookie name ${secretPrefix name}_ACCESS_TOKEN
      ${lib.concatMapStringsSep "\n" mkAclRule (effectiveGroups app)}
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
  mkHealthCheck =
    app:
    lib.optionalString (app.healthCheckPath != null) ''
      handle /_health/gatus {
        route {
          ${mkHeaderScrub app}
          rewrite * ${app.healthCheckPath}
          reverse_proxy ${app.upstream}
        }
      }
    '';
  mkApplicationRoute = name: app: ''
    @protected_${appId name} host ${lib.concatStringsSep " " (routeHosts app)}
    handle @protected_${appId name} {
      ${mkHealthCheck app}
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
  responseId = path: builtins.substring 0 12 (builtins.hashString "sha256" path);
  mkPlainStaticResponse =
    name: path: response:
    let
      matcher = "@plain_${appId name}_${responseId path}";
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
      handlers =
        if route.handlerConfig != null then
          route.handlerConfig
        else
          ''
            ${lib.optionalString (
              route.upstream != null && route.webSockets
            ) "# WebSocket upgrades are handled by Caddy reverse_proxy."}
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
          '';
      accessControlledHandlers =
        if route.allowedRemoteIPs == [ ] then
          handlers
        else
          ''
            @plain_${appId name}_allowed remote_ip ${lib.concatStringsSep " " route.allowedRemoteIPs}
            handle @plain_${appId name}_allowed {
              ${handlers}
            }
            respond 403
          '';
    in
    ''
      @plain_${appId name} host ${lib.concatStringsSep " " (routeHosts route)}
      handle @plain_${appId name} {
        ${accessControlledHandlers}
      }
    '';

  mkRouteConfig =
    routeAttrs: applicationAttrs: rejectHttp3Hosts: fallback:
    let
      errorHandlerConfig = lib.concatStringsSep "\n" (
        lib.mapAttrsToList (
          _: route: lib.optionalString (route.errorHandlerConfig != null) route.errorHandlerConfig
        ) routeAttrs
      );
    in
    ''
      ${lib.optionalString (rejectHttp3Hosts != [ ]) ''
        @reject_http3 {
          protocol http/3
          host ${lib.concatStringsSep " " rejectHttp3Hosts}
        }
        respond @reject_http3 421
        @without_http3 host ${lib.concatStringsSep " " rejectHttp3Hosts}
        header @without_http3 -Alt-Svc
      ''}
      route {
        ${lib.concatStringsSep "\n" (lib.mapAttrsToList mkPlainRoute routeAttrs)}
        ${lib.concatStringsSep "\n" (lib.mapAttrsToList mkApplicationRoute applicationAttrs)}
        ${fallback}
      }
      ${lib.optionalString (errorHandlerConfig != "") ''
        handle_errors {
          ${errorHandlerConfig}
        }
      ''}
    '';

  mkAccessLog = ''
    log {
      output file ${cfg.edge.accessLog} {
        roll_size 100MiB
        roll_keep 10
        roll_keep_for 720h
      }
      format json
    }
  '';
  managedSiteAddresses =
    subjects: port:
    lib.concatStringsSep ", " (map (subject: "https://${subject}:${toString port}") subjects);
  hostMatchesDomain = domain: host: host == domain || lib.hasSuffix ".${domain}" host;
  certificateDomains = cfg.edge.certificateDomains;
  certificateHosts = cfg.edge.certificateHosts;
  certificateSubjectsForDomain = domain: [
    domain
    "*.${domain}"
    "*.int.${domain}"
  ];
  certificateSubjects =
    lib.concatMap certificateSubjectsForDomain certificateDomains ++ certificateHosts;
  certificateDomainsForHost =
    host: lib.filter (domain: hostMatchesDomain domain host) certificateDomains;
  certificateSourcesForHost =
    host:
    map (domain: "domain:${domain}") (certificateDomainsForHost host)
    ++ lib.optional (builtins.elem host certificateHosts) "host:${host}";
  plainPublicHosts = lib.concatMap routeHosts (builtins.attrValues plainRoutes);
  protectedPublicHosts = lib.concatMap routeHosts (builtins.attrValues protectedRoutes);
  publicHosts = plainPublicHosts ++ protectedPublicHosts;
  directWanRoutes = lib.filterAttrs (_: route: route.directWan) plainRoutes;
  directWanRouteNames = builtins.attrNames directWanRoutes;
  directWanRoute =
    if builtins.length directWanRouteNames == 1 then
      builtins.head (builtins.attrValues directWanRoutes)
    else
      null;
  directWanMatchingDomains =
    if directWanRoute == null then [ ] else certificateDomainsForHost directWanRoute.publicHost;
  directWanRouteHasCertificate =
    directWanRoute != null
    && (
      builtins.length directWanMatchingDomains == 1
      || builtins.elem directWanRoute.publicHost certificateHosts
    );
  mkPublicSite =
    address: hostMatches:
    let
      routeAttrs = lib.filterAttrs (_: route: lib.any hostMatches (routeHosts route)) plainRoutes;
      applicationAttrs = lib.filterAttrs (_: app: lib.any hostMatches (routeHosts app)) protectedRoutes;
      rejectHttp3Hosts =
        lib.concatMap routeHosts (lib.filter (route: !route.http3) (builtins.attrValues routeAttrs))
        ++ lib.concatMap routeHosts (lib.filter (app: !app.http3) (builtins.attrValues applicationAttrs));
    in
    ''
      ${address} {
        @unknown_host not host ${lib.concatStringsSep " " publicHosts}
        respond @unknown_host 421
        ${mkAccessLog}
        ${mkRouteConfig routeAttrs applicationAttrs rejectHttp3Hosts "respond 421"}
      }
    '';
  publicSiteConfig = lib.concatStringsSep "\n" (
    map (
      domain:
      mkPublicSite (managedSiteAddresses (certificateSubjectsForDomain domain) cfg.edge.httpsPort) (
        hostMatchesDomain domain
      )
    ) certificateDomains
    ++ map (
      host: mkPublicSite "https://${host}:${toString cfg.edge.httpsPort}" (candidate: candidate == host)
    ) certificateHosts
  );
  directWanSiteConfig =
    lib.optionalString (cfg.edge.directWan.enable && directWanRouteHasCertificate)
      (
        let
          routeName = builtins.head directWanRouteNames;
          route = directWanRoute;
          address =
            if builtins.elem route.publicHost certificateHosts then
              "https://${route.publicHost}:${toString cfg.edge.directWan.listenPort}"
            else
              managedSiteAddresses (certificateSubjectsForDomain (builtins.head directWanMatchingDomains)) cfg.edge.directWan.listenPort;
        in
        ''
          ${address} {
            bind ${directWanListenAddress}
            ${mkAccessLog}
            ${mkRouteConfig { ${routeName} = route; } { } [ ] "abort"}
          }
        ''
      );
  allPublicHosts = publicHosts;
  caddyEnvironmentFile = "/run/caddy-env/caddy.env";
  environmentCredentials = {
    DESEC_API_TOKEN = "op://home-ops-prod/desec/api-token";
  }
  // lib.listToAttrs (
    lib.concatMap (name: [
      (lib.nameValuePair (clientSecretEnv name) "op://home-ops-prod/${name}/oidc-client-secret")
      (lib.nameValuePair (signingKeyEnv name) "op://home-ops-prod/${name}/caddy-security-signing-key")
    ]) protectedNames
  );
  environmentSetupLines = lib.mapAttrsToList (environmentName: _: ''
    printf '%s=%s\n' '${environmentName}' "$(cat "$CREDENTIALS_DIRECTORY/${environmentName}")" >> ${caddyEnvironmentFile}
  '') environmentCredentials;
in
{
  options.modules.services.caddy = {
    routes = lib.mkOption {
      type = lib.types.attrsOf routeType;
      default = { };
      description = "Plain Caddy routes";
    };
    protectedRoutes = lib.mkOption {
      type = lib.types.attrsOf protectedRouteType;
      default = { };
      description = "Caddy routes protected by the shared caddy-security authentication policy";
    };
    edge = {
      httpsPort = lib.mkOption {
        type = lib.types.port;
        default = 443;
        description = "Public Caddy HTTPS listener port";
      };
      redirectPort = lib.mkOption {
        type = lib.types.port;
        default = 8081;
        description = "LAN HTTP-to-HTTPS redirect listener port";
      };
      accessLog = lib.mkOption {
        type = lib.types.nonEmptyStr;
        default = "/var/log/caddy/access.log";
        description = "Structured public Caddy access log path";
      };
      certificateDomains = lib.mkOption {
        type = lib.types.listOf lib.types.nonEmptyStr;
        default = [ ];
        description = "Domain suffixes for Caddy-managed apex and wildcard certificates";
      };
      certificateHosts = lib.mkOption {
        type = lib.types.listOf lib.types.nonEmptyStr;
        default = [ ];
        description = "Explicit host or wildcard subjects with separate Caddy-managed certificates";
      };
      protocols = lib.mkOption {
        type = lib.types.nonEmptyListOf (
          lib.types.enum [
            "h1"
            "h2"
            "h3"
          ]
        );
        default = [
          "h1"
          "h2"
          "h3"
        ];
        description = "HTTP protocols accepted by the public HTTPS listener";
      };
      acmeEmail = lib.mkOption {
        type = lib.types.nullOr lib.types.nonEmptyStr;
        default = null;
        description = "Contact email for Caddy's ACME account";
      };
      redirectStatus = lib.mkOption {
        type = lib.types.enum [
          301
          308
        ];
        default = 308;
        description = "HTTP status used by the public HTTPS redirect listener";
      };
      directWan = {
        enable = lib.mkEnableOption "dedicated direct-WAN Caddy listener";
        listenAddress = lib.mkOption {
          type = lib.types.nullOr lib.types.nonEmptyStr;
          default = null;
          description = "Address for the direct-WAN Caddy listener";
        };
        listenPort = lib.mkOption {
          type = lib.types.port;
          default = 8443;
          description = "Port for the direct-WAN Caddy listener";
        };
      };
    };
    auth = {
      issuerURL = lib.mkOption {
        type = lib.types.nullOr lib.types.nonEmptyStr;
        default = null;
        description = "Shared Pocket ID OIDC issuer URL";
      };
      requiredGroups = lib.mkOption {
        type = lib.types.nonEmptyListOf lib.types.nonEmptyStr;
        default = [ "admins" ];
        description = "Default Pocket ID groups required by protected routes";
      };
    };
  };

  config = lib.mkIf caddyEnabled {
    assertions = [
      {
        assertion = onepassword.enable;
        message = "Caddy 1Password credentials require the systemd credential provider.";
      }
      {
        assertion = edgeConfigured;
        message = "Caddy routes require at least one managed certificate domain or subject";
      }
      {
        assertion = cfg.edge.acmeEmail != null;
        message = "Caddy routes require an ACME account email";
      }
      {
        assertion = !hasApplications || cfg.auth.issuerURL != null;
        message = "Protected Caddy routes require a shared Pocket ID issuer URL";
      }
      {
        assertion = !hasApplications || cfg.auth.requiredGroups != [ ];
        message = "Caddy authentication requires at least one default Pocket ID group";
      }
      {
        assertion = lib.all isRouteTarget routes;
        message = "Each plain Caddy route requires exactly one of upstream, root, or handlerConfig";
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
        assertion = allUnique (map appId routeNames);
        message = "Plain Caddy route names must remain unique after Caddy identifier normalization";
      }
      {
        assertion = allUnique (map appId protectedNames);
        message = "Protected Caddy route names must remain unique after Caddy identifier normalization";
      }
      {
        assertion = allUnique (map appId routeNames ++ map appId protectedNames);
        message = "Caddy route names must remain unique across plain and protected registrations";
      }
      {
        assertion = lib.all hasValidHeaders routes;
        message = "Plain Caddy header names contain an unsupported character";
      }
      {
        assertion = allUnique publicHosts;
        message = "Caddy public hostnames and aliases must be unique";
      }
      {
        assertion = lib.all (name: builtins.match "[A-Za-z0-9_-]+" name != null) (
          routeNames ++ protectedNames
        );
        message = "Caddy route names may contain only letters, digits, underscores, and hyphens";
      }
      {
        assertion = lib.all (app: lib.hasPrefix "/" app.portalPath && app.portalPath != "/") applications;
        message = "Protected Caddy portal paths must begin with a slash and may not be the site root";
      }
      {
        assertion = lib.all (
          app: lib.all (pathPrefix: lib.hasPrefix "/" pathPrefix) app.bypassPathPrefixes
        ) applications;
        message = "Protected Caddy bypass path prefixes must begin with a slash";
      }
      {
        assertion = lib.all (
          app: app.healthCheckPath == null || lib.hasPrefix "/" app.healthCheckPath
        ) applications;
        message = "Protected Caddy health check paths must begin with a slash";
      }
      {
        assertion = lib.all (app: effectiveGroups app != [ ]) applications;
        message = "Protected Caddy routes must require at least one Pocket ID group";
      }
      {
        assertion = lib.all (
          app: lib.all validHeader (builtins.attrNames app.identityHeaders)
        ) applications;
        message = "Protected Caddy identity header names contain an unsupported character";
      }
      {
        assertion = cfg.edge.certificateDomains != [ ] || cfg.edge.certificateHosts != [ ];
        message = "public Caddy edge requires at least one managed certificate domain or subject";
      }
      {
        assertion = allUnique cfg.edge.certificateDomains;
        message = "public Caddy managed certificate domains must be unique";
      }
      {
        assertion = allUnique cfg.edge.certificateHosts;
        message = "public Caddy managed certificate hosts must be unique";
      }
      {
        assertion = allUnique certificateSubjects;
        message = "public Caddy certificate subjects must belong to exactly one source";
      }
      {
        assertion = allUnique cfg.edge.protocols;
        message = "public Caddy listener protocols must be unique";
      }
      {
        assertion = lib.all (host: builtins.length (certificateSourcesForHost host) == 1) allPublicHosts;
        message = "every public Caddy hostname must match exactly one managed certificate source";
      }
      {
        assertion = lib.hasPrefix "/var/log/caddy/" cfg.edge.accessLog;
        message = "public Caddy access logs must use the systemd-managed /var/log/caddy directory";
      }
      {
        assertion = !cfg.edge.directWan.enable || cfg.edge.directWan.listenAddress != null;
        message = "the direct-WAN Caddy listener requires a listen address";
      }
      {
        assertion = !cfg.edge.directWan.enable || builtins.length (builtins.attrNames directWanRoutes) == 1;
        message = "the direct-WAN Caddy listener requires exactly one selected plain route";
      }
      {
        assertion = !cfg.edge.directWan.enable || directWanRouteHasCertificate;
        message = "the direct-WAN route must match exactly one certificate source";
      }
    ];

    modules.services.onepassword-systemd-credentials.consumers.caddy-env-setup = environmentCredentials;

    systemd.services.caddy-env-setup = {
      description = "Prepare Caddy environment from 1Password credentials";
      before = [ "caddy.service" ];
      requiredBy = [ "caddy.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        RuntimeDirectory = "caddy-env";
        UMask = "0077";
      };
      script = ''
        : > ${caddyEnvironmentFile}
        ${lib.concatStrings environmentSetupLines}
      '';
    };

    environment.persistence."/persist".directories = [ "/var/lib/caddy" ];
    networking.firewall.allowedTCPPorts = [
      cfg.edge.httpsPort
      cfg.edge.redirectPort
    ];
    networking.firewall.allowedUDPPorts = lib.optional (builtins.elem "h3" cfg.edge.protocols) cfg.edge.httpsPort;

    services.caddy = {
      enable = true;
      package = pkgs.caddy-with-security;
      environmentFile = caddyEnvironmentFile;
      openFirewall = false;
      globalConfig = ''
        auto_https disable_redirects
        email ${acmeEmail}
        cert_issuer acme {
          dir https://acme-v02.api.letsencrypt.org/directory
          email ${acmeEmail}
          dns desec {
            token {env.DESEC_API_TOKEN}
          }
          propagation_delay 5m
          propagation_timeout 12m
          resolvers ns.desec.ch:53 ns.desec.cz:53 ns.desec.li:53 ns1.desec.io:53 ns2.desec.org:53
        }
        admin 127.0.0.1:2019
        servers :${toString cfg.edge.httpsPort} {
          protocols ${lib.concatStringsSep " " cfg.edge.protocols}
          strict_sni_host on
        }
        ${lib.optionalString cfg.edge.directWan.enable ''
          servers ${directWanListenAddress}:${toString cfg.edge.directWan.listenPort} {
            protocols h1 h2
            strict_sni_host on
          }
        ''}
        ${lib.optionalString hasApplications ''
          security {
            ${lib.concatStringsSep "\n" (lib.mapAttrsToList mkProvider protectedRoutes)}
            ${lib.concatStringsSep "\n" (lib.mapAttrsToList mkPortal protectedRoutes)}
            ${lib.concatStringsSep "\n" (lib.mapAttrsToList mkPolicy protectedRoutes)}
          }
        ''}
      '';
      extraConfig = ''
        http://:${toString cfg.edge.redirectPort} {
          redir https://{http.request.host}{http.request.uri} ${toString cfg.edge.redirectStatus}
        }
        ${publicSiteConfig}
        ${directWanSiteConfig}
      '';
    };

    systemd.services.caddy = {
      requires = [ "caddy-env-setup.service" ];
      after = [ "caddy-env-setup.service" ];
      unitConfig.RequiresMountsFor = [ "/var/lib/caddy" ];
      serviceConfig = {
        CapabilityBoundingSet = [ "CAP_NET_BIND_SERVICE" ];
        AmbientCapabilities = [ "CAP_NET_BIND_SERVICE" ];
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
    boot.kernel.sysctl."net.core.rmem_max" = lib.mkDefault 7500000;
    boot.kernel.sysctl."net.core.wmem_max" = lib.mkDefault 7500000;
  };
}
