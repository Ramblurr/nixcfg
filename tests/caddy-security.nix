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
        options.modules.services.authentik.ports.http = lib.mkOption {
          type = lib.types.port;
          default = 9000;
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
        sops.age.keyFile = "/tmp/age-key.txt";

        modules.services.caddy-security = {
          enable = true;
          environmentFile = "/run/secrets/rendered/caddy-security.env";
          edge = {
            enable = true;
            certificates."example.test" = {
              certificateFile = "/var/lib/acme/example.test/fullchain.pem";
              keyFile = "/var/lib/acme/example.test/key.pem";
            };
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
                X-authentik-username = "userinfo|preferred_username";
                X-authentik-email = "email";
                X-authentik-groups = "roles";
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

  cfg = evaluated.config;
  caddy = cfg.services.caddy;
  caddyService = cfg.systemd.services.caddy;
  generatedConfig = caddy.configFile;
  failedAssertions = map (entry: entry.message) (lib.filter (entry: !entry.assertion) cfg.assertions);
in
assert lib.assertMsg (
  failedAssertions == [ ]
) "failed NixOS assertions: ${lib.concatStringsSep "; " failedAssertions}";
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
assert cfg.users.groups.acme.members == [ "caddy" ];
assert cfg.security.acme.defaults.reloadServices == [ "caddy.service" ];
assert caddyService.serviceConfig.AmbientCapabilities == [ "CAP_NET_BIND_SERVICE" ];
assert caddyService.serviceConfig.CapabilityBoundingSet == [ "CAP_NET_BIND_SERVICE" ];
assert caddyService.unitConfig.RequiresMountsFor == [ "/var/lib/acme" ];
assert lib.hasInfix "auto_https off" caddy.globalConfig;
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
assert lib.hasInfix ''inject header X-authentik-username from "userinfo|preferred_username"''
  caddy.globalConfig;
assert lib.hasInfix ''match role "books"'' caddy.globalConfig;
assert lib.hasInfix ''match role "editors"'' caddy.globalConfig;
assert lib.hasInfix "http://:18080" caddy.extraConfig;
assert lib.hasInfix "bind 127.0.0.1" caddy.extraConfig;
assert lib.hasInfix "http://:8081" caddy.extraConfig;
assert lib.hasInfix "https://alpha.example.test:443" caddy.extraConfig;
assert lib.hasInfix "https://beta.example.test:443" caddy.extraConfig;
assert lib.hasInfix "alpn h1 h2" caddy.extraConfig;
assert lib.hasInfix "@http3 protocol http/3" caddy.extraConfig;
assert lib.hasInfix "respond @http3 421" caddy.extraConfig;
assert lib.hasInfix "header -Alt-Svc" caddy.extraConfig;
assert lib.hasInfix "https://example.test:443, https://*.example.test:443" caddy.extraConfig;
assert lib.hasInfix "https://jelly.example.test:8443" caddy.extraConfig;
assert lib.hasInfix "https://example.test:8443, https://*.example.test:8443" caddy.extraConfig;
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
assert lib.hasInfix "request_header -X-authentik-*" caddy.extraConfig;
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
    ! grep -Fq 'test-alpha-secret' ${generatedConfig}
    ! grep -Fq 'test-beta-secret' ${generatedConfig}

    export ALPHA_OIDC_SECRET=test-alpha-secret
    export ALPHA_SIGNING_KEY=test-alpha-signing-key
    export BETA_OIDC_SECRET=test-beta-secret
    export BETA_SIGNING_KEY=test-beta-signing-key
    caddy adapt --adapter caddyfile --config ${generatedConfig} > "$TMPDIR/caddy.json"

    cat > "$TMPDIR/desec.Caddyfile" <<'EOF'
    {
      auto_https off
      acme_dns desec {
        token {env.DESEC_TOKEN}
      }
    }
    http://localhost:9876 {
      respond "ok"
    }
    EOF
    caddy adapt --adapter caddyfile --config "$TMPDIR/desec.Caddyfile" > "$TMPDIR/desec.json"
    grep -Fq '"name":"desec"' "$TMPDIR/desec.json"
    grep -Fq '"token":"{env.DESEC_TOKEN}"' "$TMPDIR/desec.json"

    touch "$out"
  ''
