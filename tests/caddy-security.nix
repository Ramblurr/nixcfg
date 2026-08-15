{ inputs, pkgs }:

let
  inherit (pkgs) lib;
  evaluated = inputs.nixpkgs.lib.nixosSystem {
    system = pkgs.stdenv.hostPlatform.system;
    modules = [
      ../modules/services/caddy.nix
      ../modules/services/caddy-security.nix
      ../modules/services/ingress.nix
      inputs.impermanence.nixosModules.impermanence
      inputs.sops-nix.nixosModules.sops
      ({ lib, ... }: {
        options.repo.secrets.global.email.acme = lib.mkOption {
          type = lib.types.str;
          default = "admin@example.test";
        };
        options.repo.secrets.global.domain.home = lib.mkOption {
          type = lib.types.str;
          default = "home.test";
        };
      })
      {
        nixpkgs.pkgs = pkgs;
        system.stateVersion = "26.05";
        boot.loader.grub.devices = [ "nodev" ];
        fileSystems."/" = {
          device = "none";
          fsType = "tmpfs";
        };
        repo.secrets.global.email.acme = "admin@example.test";
        repo.secrets.global.domain.home = "home.test";
        services.ncps.server.addr = "127.0.0.1:3400";
        sops.age.keyFile = "/tmp/age-key.txt";

        modules.services.caddy-security = {
          enable = true;
          environmentFile = "/run/secrets/rendered/caddy-security.env";
          edge = {
            enable = true;
            certificateDomains = [ "example.test" ];
            acmeEmail = "admin@example.test";
            directWan = {
              enable = true;
              listenAddress = "192.0.2.1";
            };
          };
          applications = {
            alpha = {
              publicHost = "alpha.example.test";
              upstream = "127.0.0.1:8001";
              oidc = {
                issuerURL = "https://id.example.test";
                clientID = "alpha-client-id";
                clientSecretEnv = "ALPHA_OIDC_SECRET";
                realm = "alpha-pocket-id";
              };
              signingKeyEnv = "ALPHA_SIGNING_KEY";
              cookiePrefix = "ALPHA";
              requiredGroups = [ "books" ];
              bypassPathPrefixes = [ "/opds" ];
              identityHeaders = {
                Remote-User = "userinfo|preferred_username";
                Remote-Name = "userinfo|preferred_username";
                Remote-Email = "email";
                Remote-Groups = "roles";
                X-Upstream-User = "userinfo|preferred_username";
              };
            };
            beta = {
              publicHost = "beta.example.test";
              upstream = "127.0.0.1:8002";
              portalPath = "/login";
              oidc = {
                issuerURL = "https://id.example.test";
                clientID = "beta-client-id";
                clientSecretEnv = "BETA_OIDC_SECRET";
                realm = "beta-pocket-id";
              };
              signingKeyEnv = "BETA_SIGNING_KEY";
              cookiePrefix = "BETA";
              requiredGroups = [ "editors" ];
              identityHeaders.Remote-User = "userinfo|preferred_username";
              http3 = false;
            };
          };
        };

        modules.services.caddy.routes = {
          atuin = {
            publicHost = "atuin.example.test";
            upstream = "http://127.0.0.1:10011";
            requestBodyMaxSize = "10MB";
          };
          audiobookshelf = {
            publicHost = "audiobookshelf.example.test";
            upstream = "http://127.0.0.1:10012";
          };
          ci = {
            publicHost = "ci.example.test";
            upstream = "http://debord.example.test:10021";
            requestBodyMaxSize = "25MB";
            responseHeaders.X-Robots-Tag = "noindex, nofollow, noarchive";
            dialTimeout = "120s";
            flushInterval = "-1";
            staticResponses."/robots.txt" = {
              body = "User-agent: *\nDisallow: /\n";
            };
          };
          clients = {
            publicHost = "clients.example.test";
            upstream = "http://127.0.0.1:10013";
          };
          data = {
            publicHost = "data.example.test";
            upstream = "http://192.0.2.2:9996";
          };
          forgejo = {
            publicHost = "git.example.test";
            upstream = "unix//run/forgejo/forgejo.sock";
            requestBodyMaxSize = "10MB";
          };
          influxdb = {
            publicHost = "influxdb.example.test";
            upstream = "http://127.0.0.1:10009";
          };
          jellyfin = {
            publicHost = "jelly.example.test";
            upstream = "http://127.0.0.1:8096";
            requestBodyMaxSize = "10MB";
            directWan = true;
          };
          paperless = {
            publicHost = "paperless.example.test";
            upstream = "http://127.0.0.1:9995";
          };
          paseo = {
            publicHost = "paseo.example.test";
            upstream = "http://quine.example.test:6767";
            requestBodyMaxSize = "100MB";
            requestHeaders = {
              Host = "{http.request.host}";
              X-Forwarded-Proto = "https";
            };
            dialTimeout = "120s";
            flushInterval = "-1";
          };
          pdf = {
            publicHost = "pdf.example.test";
            upstream = "http://127.0.0.1:10016";
            requestBodyMaxSize = "10MB";
          };
          qbittorrent = {
            publicHost = "qbittorrent.example.test";
            upstream = "http://127.0.0.1:10019";
            requestBodyMaxSize = "10MB";
          };
          static-root = {
            publicHost = "static.example.test";
            root = "/srv/plain-static";
            webSockets = false;
          };
          special = {
            publicHost = "special.example.test";
            handlerConfig = ''
              respond /special "special" 200
            '';
            errorHandlerConfig = ''
              @special_error {
                host special.example.test
                expression {http.error.status_code} == 502
              }
              respond @special_error "special unavailable" 503
            '';
          };
        };

        modules.services.ingress = {
          enable = true;
          directWan = {
            enable = true;
            listenAddress = "192.0.2.1";
          };
          domains."example.test" = { };
        };
      }
    ];
  };

  legacyCfg =
    (evaluated.extendModules {
      modules = [
        { modules.services.caddy-security.edge.enable = lib.mkForce false; }
      ];
    }).config;
  exactHostEvaluated = evaluated.extendModules {
    modules = [
      {
        modules.services.caddy-security = {
          applications = lib.mkForce { };
          edge = {
            certificateDomains = lib.mkForce [ ];
            certificateHosts = [
              "home.example.test"
              "hindsight.example.test"
            ];
            redirectStatus = 301;
            directWan.enable = lib.mkForce false;
          };
        };
        modules.services.caddy.routes = lib.mkForce {
          home-assistant = {
            publicHost = "home.example.test";
            upstream = "http://192.0.2.25:8123";
          };
          hindsight = {
            publicHost = "hindsight.example.test";
            upstream = "http://127.0.0.1:9999";
          };
        };
        modules.services.ingress.directWan.enable = lib.mkForce false;
      }
    ];
  };
  maliEvaluated = evaluated.extendModules {
    modules = [
      ../hosts/mali/acme.nix
      ../hosts/mali/nginx.nix
      ../hosts/mali/caddy.nix
      (
        { config, ... }:
        {
          modules.services.caddy-security = {
            environmentFile = lib.mkForce config.sops.templates.caddy-security-env.path;
            edge = {
              enable = lib.mkOverride 60 true;
              directWan.enable = lib.mkForce false;
            };
          };
          modules.services.ingress.directWan.enable = lib.mkForce false;
          modules.services.ingress.enable = lib.mkForce false;
        }
      )
    ];
  };
  exactHostCfg = exactHostEvaluated.config;
  transitionalExactHostCfg =
    (exactHostEvaluated.extendModules {
      modules = [
        { modules.services.ingress.legacyAcme.enable = true; }
      ];
    }).config;
  exactHostCaddy = exactHostCfg.services.caddy;
  exactHostGeneratedConfig = exactHostCaddy.configFile;
  exactHostFailedAssertions = map (entry: entry.message) (
    lib.filter (entry: !entry.assertion) exactHostCfg.assertions
  );
  maliCfg = maliEvaluated.config;
  maliPreparationCfg =
    (maliEvaluated.extendModules {
      modules = [
        { modules.services.caddy-security.edge.enable = lib.mkForce false; }
      ];
    }).config;
  maliCutoverCfg =
    (maliEvaluated.extendModules {
      modules = [
        { modules.services.ingress.legacyAcme.enable = lib.mkForce true; }
      ];
    }).config;
  maliCaddy = maliCfg.services.caddy;
  maliGeneratedConfig = maliCaddy.configFile;
  maliPreparationGeneratedConfig = maliPreparationCfg.services.caddy.configFile;
  maliFailedAssertions = map (entry: entry.message) (
    lib.filter (entry: !entry.assertion) maliCfg.assertions
  );
  cfg = evaluated.config;
  caddy = cfg.services.caddy;
  caddyService = cfg.systemd.services.caddy;
  generatedConfig = caddy.configFile;
  failedAssertions = map (entry: entry.message) (lib.filter (entry: !entry.assertion) cfg.assertions);
in
assert lib.assertMsg (
  failedAssertions == [ ]
) "failed NixOS assertions: ${lib.concatStringsSep "; " failedAssertions}";
assert lib.assertMsg (
  exactHostFailedAssertions == [ ]
) "failed exact-host NixOS assertions: ${lib.concatStringsSep "; " exactHostFailedAssertions}";
assert lib.assertMsg (
  maliFailedAssertions == [ ]
) "failed Mali NixOS assertions: ${lib.concatStringsSep "; " maliFailedAssertions}";
assert !maliCfg.services.nginx.enable;
assert !maliCfg.modules.services.ingress.legacyAcme.enable;
assert maliCfg.security.acme.certs == { };
assert builtins.elem 443 maliCfg.networking.firewall.allowedUDPPorts;
assert maliPreparationCfg.services.nginx.enable;
assert builtins.length (builtins.attrNames maliPreparationCfg.security.acme.certs) == 2;
assert !(builtins.elem 443 maliPreparationCfg.networking.firewall.allowedUDPPorts);
assert maliCutoverCfg.services.nginx.enable == false;
assert maliCutoverCfg.modules.services.ingress.legacyAcme.enable;
assert builtins.length (builtins.attrNames maliCutoverCfg.security.acme.certs) == 2;
assert
  maliCfg.modules.services.caddy-security.edge.protocols == [
    "h1"
    "h2"
    "h3"
  ];
assert
  maliCfg.modules.services.caddy-security.edge.certificateHosts == [
    "attic.mgmt.home.test"
    "attic.int.home.test"
    "nix-cache.int.home.test"
    "s3.data.home.test"
    "*.s3.data.home.test"
    "minio.data.home.test"
    "*.s3.mgmt.home.test"
    "minio.mgmt.home.test"
    "s3.mgmt.home.test"
  ];
assert lib.hasInfix "servers :443 {" maliCaddy.globalConfig;
assert lib.hasInfix "protocols h1 h2 h3" maliCaddy.globalConfig;
assert lib.hasInfix "http://:80" maliCaddy.extraConfig;
assert lib.hasInfix "https://attic.mgmt.home.test:443" maliCaddy.extraConfig;
assert lib.hasInfix "https://attic.int.home.test:443" maliCaddy.extraConfig;
assert lib.hasInfix "https://*.s3.data.home.test:443" maliCaddy.extraConfig;
assert !lib.hasInfix "*.int.s3.data.home.test" maliCaddy.extraConfig;
assert lib.hasInfix "@plain_minio_console host minio.data.home.test minio.mgmt.home.test"
  maliCaddy.extraConfig;
assert lib.hasInfix "@plain_minio_console_allowed remote_ip 10.9.8.0/23 10.9.10.0/23"
  maliCaddy.extraConfig;
assert lib.hasInfix "respond 403" maliCaddy.extraConfig;
assert lib.hasInfix "@plain_s3 host s3.data.home.test s3.mgmt.home.test" maliCaddy.extraConfig;
assert lib.hasInfix "handle_path /minio/ui/*"
  maliCfg.modules.services.caddy.routes.minio-console.handlerConfig;
assert lib.hasInfix "header_up X-NginX-Proxy \"true\""
  maliCfg.modules.services.caddy.routes.minio-console.handlerConfig;
assert lib.hasInfix "header_up X-Real-IP {http.request.remote.host}"
  maliCfg.modules.services.caddy.routes.minio-console.handlerConfig;
assert lib.hasInfix "header_up X-Forwarded-Proto \"https\""
  maliCfg.modules.services.caddy.routes.minio-console.handlerConfig;
assert lib.hasInfix "flush_interval -1"
  maliCfg.modules.services.caddy.routes.minio-console.handlerConfig;
assert lib.hasInfix "dial_timeout 300s"
  maliCfg.modules.services.caddy.routes.minio-console.handlerConfig;
assert maliCfg.modules.services.caddy.routes.s3.requestBodyMaxSize == null;
assert maliCfg.modules.services.caddy.routes.s3.flushInterval == "-1";
assert maliCfg.modules.services.caddy.routes.s3.dialTimeout == "300s";
assert
  maliCfg.modules.services.caddy.routes.s3.requestHeaders == {
    X-Forwarded-Proto = "https";
    X-Real-IP = "{http.request.remote.host}";
  };
assert
  maliCfg.modules.services.caddy.routes.s3.allowedRemoteIPs == [
    "10.9.8.0/23"
    "10.9.10.0/23"
  ];
assert maliCfg.sops.templates.caddy-security-env.owner == "caddy";
assert maliCfg.sops.templates.caddy-security-env.group == "caddy";
assert maliCfg.sops.templates.caddy-security-env.mode == "0400";
assert
  maliCfg.sops.templates.caddy-security-env.content
  == "DESEC_API_TOKEN=${maliCfg.sops.placeholder.desec_api_token}";
assert caddy.enable;
assert caddy.package == pkgs.caddy-with-security;
assert !caddy.openFirewall;
assert caddy.environmentFile == "/run/secrets/rendered/caddy-security.env";
assert !(builtins.elem 18080 cfg.networking.firewall.allowedTCPPorts);
assert builtins.elem "sops-install-secrets.service" caddyService.after;
assert builtins.elem "sops-install-secrets.service" caddyService.requires;
assert caddyService.serviceConfig.ProtectSystem == "strict";
assert !cfg.services.nginx.enable;
assert builtins.attrNames cfg.services.nginx.virtualHosts == [ "localhost" ];
assert cfg.users.groups.acme.members == [ ];
assert cfg.security.acme.certs == { };
assert cfg.security.acme.defaults.reloadServices == [ ];
assert
  !(builtins.elem "/var/lib/acme" (
    map (entry: entry.directory) cfg.environment.persistence."/persist".directories
  ));
assert cfg.sops.secrets ? desec_api_token;
assert legacyCfg.services.nginx.enable;
assert legacyCfg.users.groups.acme.members == [ "nginx" ];
assert legacyCfg.security.acme.defaults.reloadServices == [ "nginx.service" ];
assert builtins.elem "/var/lib/acme" (
  map (entry: entry.directory) legacyCfg.environment.persistence."/persist".directories
);
assert exactHostCaddy.enable;
assert !exactHostCfg.services.nginx.enable;
assert exactHostCfg.modules.services.ingress.legacyAcme.enable == false;
assert exactHostCfg.users.groups.acme.members == [ ];
assert exactHostCfg.security.acme.certs == { };
assert
  !builtins.elem "/var/lib/acme" (
    map (entry: entry.directory) exactHostCfg.environment.persistence."/persist".directories
  );
assert transitionalExactHostCfg.modules.services.ingress.legacyAcme.enable;
assert !transitionalExactHostCfg.services.nginx.enable;
assert transitionalExactHostCfg.users.groups.acme.members == [ "caddy" ];
assert transitionalExactHostCfg.security.acme.certs ? "example.test";
assert builtins.elem "/var/lib/acme" (
  map (entry: entry.directory) transitionalExactHostCfg.environment.persistence."/persist".directories
);
assert lib.hasInfix "https://home.example.test:443" exactHostCaddy.extraConfig;
assert lib.hasInfix "https://hindsight.example.test:443" exactHostCaddy.extraConfig;
assert !lib.hasInfix "https://*.example.test:443" exactHostCaddy.extraConfig;
assert lib.hasInfix "redir https://{http.request.host}{http.request.uri} 301"
  exactHostCaddy.extraConfig;
assert !lib.hasInfix "security {" exactHostCaddy.globalConfig;
assert lib.hasInfix "@plain_home_assistant host home.example.test" exactHostCaddy.extraConfig;
assert lib.hasInfix "@plain_hindsight host hindsight.example.test" exactHostCaddy.extraConfig;
assert lib.hasInfix "respond @unknown_host 421" exactHostCaddy.extraConfig;
assert caddyService.serviceConfig.AmbientCapabilities == [ "CAP_NET_BIND_SERVICE" ];
assert caddyService.serviceConfig.CapabilityBoundingSet == [ "CAP_NET_BIND_SERVICE" ];
assert caddyService.unitConfig.RequiresMountsFor == [ "/var/lib/caddy" ];
assert builtins.elem "/var/lib/caddy" (
  map (entry: entry.directory) cfg.environment.persistence."/persist".directories
);
assert lib.hasInfix "auto_https disable_redirects" caddy.globalConfig;
assert lib.hasInfix "trusted_proxies static 127.0.0.1/32 ::1/128" caddy.globalConfig;
assert lib.hasInfix "servers :443" caddy.globalConfig;
assert lib.hasInfix "strict_sni_host on" caddy.globalConfig;
assert lib.hasInfix "servers 192.0.2.1:8443" caddy.globalConfig;
assert lib.hasInfix "oauth identity provider alpha_pocket_id" caddy.globalConfig;
assert lib.hasInfix "oauth identity provider beta_pocket_id" caddy.globalConfig;
assert lib.hasInfix "client_id alpha-client-id" caddy.globalConfig;
assert lib.hasInfix "client_id beta-client-id" caddy.globalConfig;
assert lib.hasInfix "client_secret {env.ALPHA_OIDC_SECRET}" caddy.globalConfig;
assert lib.hasInfix "client_secret {env.BETA_OIDC_SECRET}" caddy.globalConfig;
assert lib.hasInfix "crypto key sign-verify {env.ALPHA_SIGNING_KEY}" caddy.globalConfig;
assert lib.hasInfix "crypto key sign-verify {env.BETA_SIGNING_KEY}" caddy.globalConfig;
assert lib.hasInfix "authentication portal alpha_portal" caddy.globalConfig;
assert lib.hasInfix "authentication portal beta_portal" caddy.globalConfig;
assert lib.hasInfix "authorization policy alpha_policy" caddy.globalConfig;
assert lib.hasInfix "authorization policy beta_policy" caddy.globalConfig;
assert lib.hasInfix "set cookie name prefix ALPHA" caddy.globalConfig;
assert lib.hasInfix "set cookie name prefix BETA" caddy.globalConfig;
assert lib.hasInfix "set auth url /auth/oauth2/alpha-pocket-id" caddy.globalConfig;
assert lib.hasInfix "set auth url /login/oauth2/beta-pocket-id" caddy.globalConfig;
assert lib.hasInfix ''inject header X-Upstream-User from "userinfo|preferred_username"''
  caddy.globalConfig;
assert lib.hasInfix ''match role "books"'' caddy.globalConfig;
assert lib.hasInfix ''match role "editors"'' caddy.globalConfig;
assert lib.hasInfix "http://:18080" caddy.extraConfig;
assert lib.hasInfix "bind 127.0.0.1" caddy.extraConfig;
assert lib.hasInfix "http://:8081" caddy.extraConfig;
assert lib.hasInfix
  "https://example.test:443, https://*.example.test:443, https://*.int.example.test:443"
  caddy.extraConfig;
assert lib.hasInfix "@reject_http3" caddy.extraConfig;
assert lib.hasInfix "protocol http/3" caddy.extraConfig;
assert lib.hasInfix "host beta.example.test" caddy.extraConfig;
assert lib.hasInfix "respond @reject_http3 421" caddy.extraConfig;
assert lib.hasInfix "header @without_http3 -Alt-Svc" caddy.extraConfig;
assert lib.hasInfix
  "https://example.test:8443, https://*.example.test:8443, https://*.int.example.test:8443"
  caddy.extraConfig;
assert lib.hasInfix "cert_issuer acme" caddy.globalConfig;
assert lib.hasInfix "dir https://acme-v02.api.letsencrypt.org/directory" caddy.globalConfig;
assert lib.hasInfix "email admin@example.test" caddy.globalConfig;
assert lib.hasInfix "token {env.DESEC_API_TOKEN}" caddy.globalConfig;
assert lib.hasInfix "propagation_delay 5m" caddy.globalConfig;
assert lib.hasInfix "propagation_timeout 12m" caddy.globalConfig;
assert lib.hasInfix
  "resolvers ns.desec.ch:53 ns.desec.cz:53 ns.desec.li:53 ns1.desec.io:53 ns2.desec.org:53"
  caddy.globalConfig;
assert !lib.hasInfix "/var/lib/acme" caddy.extraConfig;
assert lib.hasInfix "abort" caddy.extraConfig;
assert lib.hasInfix "output file /var/log/caddy/access.log" caddy.extraConfig;
assert lib.hasInfix "respond @unknown_host 421" caddy.extraConfig;
assert lib.hasInfix "handle /auth*" caddy.extraConfig;
assert lib.hasInfix "handle /login*" caddy.extraConfig;
assert lib.hasInfix "handle /opds*" caddy.extraConfig;
assert lib.hasInfix "authorize with alpha_policy" caddy.extraConfig;
assert lib.hasInfix "authorize with beta_policy" caddy.extraConfig;
assert lib.hasInfix "reverse_proxy 127.0.0.1:8001" caddy.extraConfig;
assert lib.hasInfix "reverse_proxy 127.0.0.1:8002" caddy.extraConfig;
assert lib.hasInfix "request_header -Remote-User" caddy.extraConfig;
assert lib.hasInfix "request_header -X-Upstream-User" caddy.extraConfig;
assert cfg.modules.services.caddy.routes.paseo.requestHeaders.Host == "{http.request.host}";
assert cfg.modules.services.caddy.routes.jellyfin.webSockets;
assert lib.hasInfix "@plain_atuin host atuin.example.test" caddy.extraConfig;
assert lib.hasInfix "@plain_static_root host static.example.test" caddy.extraConfig;
assert lib.hasInfix "reverse_proxy unix//run/forgejo/forgejo.sock" caddy.extraConfig;
assert lib.hasInfix "# WebSocket upgrades are handled by Caddy reverse_proxy." caddy.extraConfig;
assert lib.hasInfix "request_body {" caddy.extraConfig;
assert lib.hasInfix "max_size 25MB" caddy.extraConfig;
assert lib.hasInfix "header_up Host \"{http.request.host}\"" caddy.extraConfig;
assert lib.hasInfix "header X-Robots-Tag \"noindex, nofollow, noarchive\"" caddy.extraConfig;
assert lib.hasInfix "flush_interval -1" caddy.extraConfig;
assert lib.hasInfix "dial_timeout 120s" caddy.extraConfig;
assert lib.hasInfix "root * /srv/plain-static" caddy.extraConfig;
assert lib.hasInfix "respond @plain_ci_" caddy.extraConfig;
assert lib.hasInfix "@plain_special host special.example.test" caddy.extraConfig;
assert lib.hasInfix ''respond /special "special" 200'' caddy.extraConfig;
assert lib.hasInfix "handle_errors {" caddy.extraConfig;
assert lib.hasInfix ''respond @special_error "special unavailable" 503'' caddy.extraConfig;
assert lib.hasInfix "respond @unknown_host 421" caddy.extraConfig;
assert lib.hasInfix "atuin.example.test" caddy.extraConfig;
assert !lib.hasInfix "PLAIN_OIDC" caddy.extraConfig;
pkgs.runCommand "caddy-security-test"
  {
    nativeBuildInputs = [
      pkgs.caddy-with-security
      pkgs.gnugrep
    ];
  }
  ''
    caddy version > "$TMPDIR/version"
    caddy build-info > "$TMPDIR/build-info"
    caddy list-modules > "$TMPDIR/modules"

    grep -Eq '^dep[[:space:]]+github\.com/greenpau/caddy-security[[:space:]]+v1\.1\.64([[:space:]]|$)' \
      "$TMPDIR/build-info"
    grep -Eq '^dep[[:space:]]+github\.com/caddy-dns/desec[[:space:]]+v1\.1\.0([[:space:]]|$)' \
      "$TMPDIR/build-info"
    grep -Fxq security "$TMPDIR/modules"
    grep -Fxq http.handlers.authenticator "$TMPDIR/modules"
    grep -Fxq http.authentication.providers.authorizer "$TMPDIR/modules"
    grep -Fxq dns.providers.desec "$TMPDIR/modules"
    ! grep -Fqi frankenphp "$TMPDIR/modules"

    grep -Fq '{env.ALPHA_OIDC_SECRET}' ${generatedConfig}
    grep -Fq '{env.BETA_OIDC_SECRET}' ${generatedConfig}
    grep -Fq '{env.DESEC_API_TOKEN}' ${generatedConfig}
    ! grep -Fq 'test-alpha-secret' ${generatedConfig}
    ! grep -Fq 'test-beta-secret' ${generatedConfig}
    ! grep -Fq 'test-desec-token' ${generatedConfig}

    export ALPHA_OIDC_SECRET=test-alpha-secret
    export ALPHA_SIGNING_KEY=test-alpha-signing-key
    export BETA_OIDC_SECRET=test-beta-secret
    export BETA_SIGNING_KEY=test-beta-signing-key
    export DESEC_API_TOKEN=test-desec-token
    caddy adapt --adapter caddyfile --config ${generatedConfig} > "$TMPDIR/caddy.json"
    grep -Fq '"name":"desec"' "$TMPDIR/caddy.json"
    grep -Fq '"token":"{env.DESEC_API_TOKEN}"' "$TMPDIR/caddy.json"

    caddy adapt --adapter caddyfile --config ${exactHostGeneratedConfig} > "$TMPDIR/exact-host-caddy.json"
    grep -Fq '"home.example.test"' "$TMPDIR/exact-host-caddy.json"
    grep -Fq '"hindsight.example.test"' "$TMPDIR/exact-host-caddy.json"
    grep -Fq '"name":"desec"' "$TMPDIR/exact-host-caddy.json"
    grep -Fq '"token":"{env.DESEC_API_TOKEN}"' "$TMPDIR/exact-host-caddy.json"

    caddy adapt --adapter caddyfile --config ${maliGeneratedConfig} > "$TMPDIR/mali-caddy.json"
    grep -Fq '"attic.mgmt.home.test"' "$TMPDIR/mali-caddy.json"
    grep -Fq '"*.s3.data.home.test"' "$TMPDIR/mali-caddy.json"
    grep -Fq '"remote_ip"' "$TMPDIR/mali-caddy.json"
    grep -Fq '"protocols":["h1","h2","h3"]' "$TMPDIR/mali-caddy.json"

    caddy adapt --adapter caddyfile --config ${maliPreparationGeneratedConfig} > "$TMPDIR/mali-preparation-caddy.json"
    grep -Fq '"listen":["127.0.0.1:18080"]' "$TMPDIR/mali-preparation-caddy.json"
    ! grep -Fq 'acme-v02.api.letsencrypt.org' "$TMPDIR/mali-preparation-caddy.json"

    touch "$out"
  ''
