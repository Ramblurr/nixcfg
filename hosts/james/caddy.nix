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
  docsDeployUser = "docs.${work}";
  inherit (config.repo.secrets.local) atprotoDid;

  socketAddress = "unix//run/caddy/james-ingress.sock|0660";
  accessLog = "/var/log/caddy/access.log";
  reportDir = "/var/lib/goaccess";
  workRoot = "/var/lib/static-web/${work}/www";
  docsBackendSocket = "unix//var/lib/static-web/${work}/docs/.run/docs-site.sock";
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

  docsHandler = ''
    handle /_deploy* {
      reverse_proxy ${docsHookSocket}
    }
    handle {
      reverse_proxy ${docsBackendSocket}
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

    users.users.caddy.extraGroups = [
      caseyLink
      docsDeployUser
    ];

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
