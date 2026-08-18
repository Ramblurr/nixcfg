{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.repo.secrets.global) code codeWork email;
  inherit (config.repo.secrets.global.domain)
    be
    caseylink
    et
    home
    ov
    partner
    work
    work2
    ;
  caseyLink = config.repo.secrets.global.domain."casey.link";
  inherit (config.repo.secrets.local) atprotoDid;

  socketAddress = "unix//run/caddy/james-ingress.sock|0660";
  accessLog = "/var/log/caddy/access.log";
  reportDir = "/var/lib/goaccess";
  workRoot = "/var/lib/static-web/${work}/www";
  docsRoot = "/var/lib/static-web/${work}/docs/current";
  docsHookSocket = "unix//var/lib/static-web/${work}/docs/.run/github-docs-hook.sock";
  workHookSocket = "unix//var/lib/static-web/${work}/.run/github-work-site-deploy-${work}.sock";
  caseyLinkSocket = "unix//var/lib/${caseyLink}/.run/site.sock";

  canonicalRedirect = ''
    redir https://${work}{http.request.uri} 301
  '';
  davRedirects = ''
    handle /.well-known/carddav {
      redir https://dav.${home}/dav/ 301
    }
    handle /.well-known/caldav {
      redir https://dav.${home}/dav/ 301
    }
  '';
  atprotoResponse = ''
    handle /.well-known/atproto-did {
      header Content-Type text/plain
      respond ${builtins.toJSON atprotoDid} 200
    }
  '';

  antoraPrefixRedirects = [
    {
      id = "latest";
      pattern = "^/latest/(.*)$";
      target = "/{re.latest.1}";
    }
    {
      id = "client_ip_latest";
      pattern = "^/ol\\.client-ip/latest/(.*)$";
      target = "/ol.client-ip/0.1/{re.client_ip_latest.1}";
    }
    {
      id = "datahike_sqlite_latest";
      pattern = "^/datahike-sqlite/latest/(.*)$";
      target = "/datahike-sqlite/next/{re.datahike_sqlite_latest.1}";
    }
    {
      id = "datastar_expressions_latest";
      pattern = "^/datastar-expressions/latest/(.*)$";
      target = "/datastar-expressions/next/{re.datastar_expressions_latest.1}";
    }
    {
      id = "datomic_pro_flake_latest";
      pattern = "^/datomic-pro-flake/latest/(.*)$";
      target = "/datomic-pro-flake/0.15/{re.datomic_pro_flake_latest.1}";
    }
    {
      id = "fluent_tooling_latest";
      pattern = "^/fluent-tooling/latest/(.*)$";
      target = "/fluent-tooling/0.0.1/{re.fluent_tooling_latest.1}";
    }
    {
      id = "h2o_zig_latest";
      pattern = "^/h2o-zig/latest/(.*)$";
      target = "/h2o-zig/next/{re.h2o_zig_latest.1}";
    }
    {
      id = "nixos_hetzner_latest";
      pattern = "^/nixos-hetzner/latest/(.*)$";
      target = "/nixos-hetzner/next/{re.nixos_hetzner_latest.1}";
    }
    {
      id = "nixos_hetzner_demo_latest";
      pattern = "^/nixos-hetzner-demo/latest/(.*)$";
      target = "/nixos-hetzner-demo/next/{re.nixos_hetzner_demo_latest.1}";
    }
    {
      id = "busker_latest";
      pattern = "^/ol\\.busker/latest/(.*)$";
      target = "/ol.busker/next/{re.busker_latest.1}";
    }
    {
      id = "clave_latest";
      pattern = "^/ol\\.clave/latest/(.*)$";
      target = "/ol.clave/next/{re.clave_latest.1}";
    }
    {
      id = "dirs_latest";
      pattern = "^/ol\\.dirs/latest/(.*)$";
      target = "/ol.dirs/0.1/{re.dirs_latest.1}";
    }
    {
      id = "llx_latest";
      pattern = "^/ol\\.llx/latest/(.*)$";
      target = "/ol.llx/next/{re.llx_latest.1}";
    }
    {
      id = "protocol53_latest";
      pattern = "^/ol\\.protocol53/latest/(.*)$";
      target = "/ol.protocol53/next/{re.protocol53_latest.1}";
    }
    {
      id = "sfv_latest";
      pattern = "^/ol\\.sfv/latest/(.*)$";
      target = "/ol.sfv/0.1/{re.sfv_latest.1}";
    }
    {
      id = "sops_latest";
      pattern = "^/ol\\.sops/latest/(.*)$";
      target = "/ol.sops/0.1/{re.sops_latest.1}";
    }
    {
      id = "trixnity_latest";
      pattern = "^/ol\\.trixnity/latest/(.*)$";
      target = "/ol.trixnity/next/{re.trixnity_latest.1}";
    }
    {
      id = "vips_latest";
      pattern = "^/ol\\.vips/latest/(.*)$";
      target = "/ol.vips/0.0.1/{re.vips_latest.1}";
    }
  ];
  antoraExactRedirects = {
    "/ol.client-ip/" = "/ol.client-ip/0.1/";
    "/datahike-sqlite/" = "/datahike-sqlite/next/";
    "/datastar-expressions/" = "/datastar-expressions/next/";
    "/datomic-pro-flake/" = "/datomic-pro-flake/0.15/";
    "/fluent-tooling/" = "/fluent-tooling/0.0.1/";
    "/h2o-zig/" = "/h2o-zig/next/";
    "/nixos-hetzner/" = "/nixos-hetzner/next/";
    "/nixos-hetzner-demo/" = "/nixos-hetzner-demo/next/";
    "/ol.busker/" = "/ol.busker/next/";
    "/ol.clave/" = "/ol.clave/next/";
    "/ol.dirs/" = "/ol.dirs/0.1/";
    "/ol.llx/" = "/ol.llx/next/";
    "/ol.protocol53/" = "/ol.protocol53/next/";
    "/ol.sfv/" = "/ol.sfv/0.1/";
    "/ol.sops/" = "/ol.sops/0.1/";
    "/ol.trixnity/" = "/ol.trixnity/next/";
    "/ol.vips/" = "/ol.vips/0.0.1/";
  };
  mkAntoraPrefixRedirect = redirect: ''
    @${redirect.id} path_regexp ${redirect.id} ${redirect.pattern}
    redir @${redirect.id} ${redirect.target} 302
  '';
  mkAntoraExactRedirect = path: target: "redir ${path} ${target} 301";
  docsHandler = ''
    ${lib.concatMapStringsSep "\n" mkAntoraPrefixRedirect antoraPrefixRedirects}
    ${lib.concatStringsSep "\n" (lib.mapAttrsToList mkAntoraExactRedirect antoraExactRedirects)}

    root * ${docsRoot}
    @docs_add_trailing_slash {
      path_regexp docs_add_trailing_slash ^.+[^/]$
      file {http.request.uri.path}/index.html
    }
    redir @docs_add_trailing_slash {http.request.uri.path}/ 301
    @docs_strip_trailing_slash {
      path_regexp docs_strip_trailing_slash ^(.+)/$
      not file {http.request.uri.path}index.html
    }
    redir @docs_strip_trailing_slash {re.docs_strip_trailing_slash.1} 301

    handle /.etc/nginx/rewrite.conf {
      respond 404
    }
    handle /_deploy* {
      reverse_proxy ${docsHookSocket}
    }
    handle {
      @docs_short_cache not path *.png *.jpg *.jpeg *.gif *.svg *.ico *.webp *.avif *.woff *.woff2 *.ttf *.otf *.eot
      header @docs_short_cache Cache-Control "public, no-transform, max-age=1800, must-revalidate"
      @docs_assets path *.png *.jpg *.jpeg *.gif *.svg *.ico *.webp *.avif *.woff *.woff2 *.ttf *.otf *.eot
      header @docs_assets Cache-Control "public, no-transform, max-age=2592000, must-revalidate"
      try_files {http.request.uri.path} {http.request.uri.path}.html {http.request.uri.path}/index.html
      file_server
    }
  '';
  docsErrorHandler = ''
    @docs_not_found expression {http.error.status_code} == 404
    handle @docs_not_found {
      rewrite * /404.html
      file_server
    }
  '';
  workHandler = ''
    handle /_deploy* {
      reverse_proxy ${workHookSocket}
    }
    ${davRedirects}
    handle /.well-known/matrix/server {
      header Content-Type application/json
      respond ${builtins.toJSON (builtins.toJSON { "m.server" = "matrix.${work}:443"; })} 200
    }
    handle /.well-known/matrix/client {
      header Content-Type application/json
      header Access-Control-Allow-Origin *
      respond ${
        builtins.toJSON (
          builtins.toJSON {
            "m.homeserver"."base_url" = "https://matrix.${work}";
          }
        )
      } 200
    }
    root * ${workRoot}
    file_server
  '';
  routes = {
    binary-elysium = {
      hosts = [
        be
        "www.${be}"
      ];
      handler = canonicalRedirect;
    };
    casey-link = {
      hosts = [
        caseyLink
        "www.${caseyLink}"
      ];
      handler = ''
        ${davRedirects}
        ${atprotoResponse}
        reverse_proxy ${caseyLinkSocket}
      '';
    };
    casey-link-code = {
      hosts = [ "code.${caseyLink}" ];
      handler = "redir ${code} 302";
    };
    caseylink = {
      hosts = [
        caseylink
        "www.${caseylink}"
      ];
      handler = ''
        ${davRedirects}
        ${canonicalRedirect}
      '';
    };
    caseylink-code = {
      hosts = [ "code.${caseylink}" ];
      handler = "redir ${code} 302";
    };
    work-code = {
      hosts = [ "code.${work}" ];
      handler = "redir ${codeWork} 302";
    };
    work-docs = {
      hosts = [ "docs.${work}" ];
      handler = docsHandler;
      errorHandler = docsErrorHandler;
    };
    elusive-truth = {
      hosts = [
        et
        "www.${et}"
      ];
      handler = canonicalRedirect;
    };
    pocket-id-work = {
      hosts = [ "id.${work}" ];
      handler = "reverse_proxy http://127.0.0.1:1412";
    };
    pocket-id-home = {
      hosts = [ "id.${home}" ];
      handler = "reverse_proxy http://127.0.0.1:1411";
    };
    goaccess = {
      hosts = [ "logs.${work}" ];
      handler = ''
        @plain_goaccess_allowed remote_ip 100.64.0.0/10
        handle @plain_goaccess_allowed {
          root * ${reportDir}
          file_server
        }
        respond 403
      '';
    };
    on-vagrancy = {
      hosts = [
        ov
        "www.${ov}"
      ];
      handler = canonicalRedirect;
    };
    work-legacy = {
      hosts = [
        work2
        "www.${work2}"
      ];
      handler = canonicalRedirect;
    };
    work = {
      hosts = [
        work
        "www.${work}"
      ];
      handler = workHandler;
    };
    partner = {
      hosts = [
        partner
        "www.${partner}"
      ];
      handler = ''
        ${davRedirects}
        root * /var/lib/static-web/${partner}
        file_server
      '';
    };
  };

  routeId = name: lib.replaceStrings [ "-" ] [ "_" ] name;
  certificateHosts = lib.concatMap (route: route.hosts) (builtins.attrValues routes);
  mkAccessLog = ''
    log {
      output file ${accessLog} {
        mode 0660
        roll_size 100MiB
        roll_keep 10
        roll_keep_for 720h
      }
      format json
    }
  '';
  mkSite =
    name: route:
    let
      matcher = "@plain_${routeId name}";
      addresses = lib.concatStringsSep ", " (map (host: "https://${host}") route.hosts);
    in
    ''
      ${addresses} {
        bind ${socketAddress}
        ${mkAccessLog}
        route {
          ${matcher} host ${lib.concatStringsSep " " route.hosts}
          handle ${matcher} {
            ${route.handler}
          }
          respond 421
        }
        ${lib.optionalString (route ? errorHandler) ''
          handle_errors {
            ${route.errorHandler}
          }
        ''}
      }
    '';
in
{
  config = {
    assertions = [
      {
        assertion = builtins.length certificateHosts == 23;
        message = "James Caddy must preserve its 23 exact certificate hosts";
      }
      {
        assertion = builtins.length (lib.unique certificateHosts) == builtins.length certificateHosts;
        message = "James Caddy certificate hosts must be unique";
      }
      {
        assertion = config.modules.services.caddy.routes == { };
        message = "James direct Caddy configuration does not consume shared route registrations";
      }
      {
        assertion = config.modules.services.caddy.protectedRoutes == { };
        message = "James direct Caddy configuration does not consume protected route registrations";
      }
    ];

    sops.secrets.desec_api_token = { };
    sops.templates.james-caddy-env = {
      owner = "caddy";
      group = "caddy";
      mode = "0400";
      restartUnits = [ "caddy.service" ];
      content = "DESEC_API_TOKEN=${config.sops.placeholder.desec_api_token}";
    };

    environment.persistence."/persist".directories = [ "/var/lib/caddy" ];

    services.caddy = {
      enable = true;
      package = pkgs.caddy-with-security;
      environmentFile = config.sops.templates.james-caddy-env.path;
      openFirewall = false;
      globalConfig = ''
        auto_https disable_redirects
        email ${email.acme}
        cert_issuer acme {
          dir https://acme-v02.api.letsencrypt.org/directory
          email ${email.acme}
          dns desec {
            token {env.DESEC_API_TOKEN}
          }
          propagation_delay 5m
          propagation_timeout 12m
          resolvers ns.desec.ch:53 ns.desec.cz:53 ns.desec.li:53 ns1.desec.io:53 ns2.desec.org:53
        }
        admin 127.0.0.1:2019
        servers ${socketAddress} {
          protocols h1 h2
          strict_sni_host on
          listener_wrappers {
            proxy_protocol {
              timeout 5s
              fallback_policy require
            }
            tls
          }
        }
      '';
      extraConfig = lib.concatStringsSep "\n" (lib.mapAttrsToList mkSite routes);
    };

    users.users.caddy.extraGroups = [ caseyLink ];

    systemd.services.caddy = {
      requires = [ "sops-install-secrets.service" ];
      after = [ "sops-install-secrets.service" ];
      unitConfig.RequiresMountsFor = [
        "/var/lib/caddy"
        "/var/lib/goaccess"
        "/var/lib/static-web"
        "/var/lib/${caseyLink}"
      ];
      serviceConfig = {
        CapabilityBoundingSet = [ ];
        AmbientCapabilities = [ ];
        RuntimeDirectory = "caddy";
        RuntimeDirectoryMode = "0750";
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
