{ inputs, pkgs }:

let
  inherit (pkgs) lib;
  evaluated = inputs.nixpkgs.lib.nixosSystem {
    system = pkgs.stdenv.hostPlatform.system;
    modules = [
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
            };
          };
        };

        modules.services.ingress = {
          enable = true;
          caddySecurity.upstream = "http://127.0.0.1:18080";
          domains."example.test" = { };
          virtualHosts = {
            "alpha.example.test" = {
              acmeHost = "example.test";
              upstream = "http://127.0.0.1:8001";
              forwardAuth = true;
              usesCaddySecurity = true;
              extraConfig = "client_max_body_size 0;";
              forwardAuthBypassPaths."/opds" = ''
                proxy_set_header Authorization $http_authorization;
              '';
            };
            "rollback.example.test" = {
              acmeHost = "example.test";
              upstream = "http://127.0.0.1:8003";
              forwardAuth = true;
              forwardAuthBypassPaths."/opds" = ''
                proxy_set_header Authorization $http_authorization;
              '';
            };
            "alpha-kobo.example.test" = {
              acmeHost = "example.test";
              upstream = "http://127.0.0.1:8001/";
            };
          };
        };
      }
    ];
  };

  cfg = evaluated.config;
  caddy = cfg.services.caddy;
  caddyService = cfg.systemd.services.caddy;
  generatedConfig = caddy.configFile;
  alphaNginx = cfg.services.nginx.virtualHosts."alpha.example.test";
  rollbackNginx = cfg.services.nginx.virtualHosts."rollback.example.test";
  koboNginx = cfg.services.nginx.virtualHosts."alpha-kobo.example.test";
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
assert lib.hasInfix "auto_https off" caddy.globalConfig;
assert lib.hasInfix "trusted_proxies static 127.0.0.1/32 ::1/128" caddy.globalConfig;
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
assert alphaNginx.useACMEHost == "example.test";
assert alphaNginx.forceSSL;
assert alphaNginx.locations."/".proxyPass == "http://127.0.0.1:18080";
assert alphaNginx.locations."/".proxyWebsockets;
assert lib.hasInfix "client_max_body_size 0;" alphaNginx.extraConfig;
assert lib.hasInfix "proxy_set_header Host $host;" alphaNginx.locations."/".extraConfig;
assert lib.hasInfix "proxy_set_header X-Forwarded-Host $host;" alphaNginx.locations."/".extraConfig;
assert lib.hasInfix "proxy_set_header X-Forwarded-Proto https;"
  alphaNginx.locations."/".extraConfig;
assert lib.hasInfix "proxy_set_header X-Forwarded-For $remote_addr;"
  alphaNginx.locations."/".extraConfig;
assert lib.hasInfix "proxy_set_header X-Forwarded-Uri $request_uri;"
  alphaNginx.locations."/".extraConfig;
assert !lib.hasInfix "auth_request" alphaNginx.locations."/".extraConfig;
assert alphaNginx.locations."/opds".proxyPass == "http://127.0.0.1:18080";
assert lib.hasInfix "auth_request off;" alphaNginx.locations."/opds".extraConfig;
assert lib.hasInfix "proxy_set_header Authorization $http_authorization;"
  alphaNginx.locations."/opds".extraConfig;
assert lib.hasInfix "proxy_set_header X-Forwarded-Host $host;"
  alphaNginx.locations."/opds".extraConfig;
assert rollbackNginx.locations."/".proxyPass == "http://127.0.0.1:8003";
assert lib.hasInfix "auth_request        /outpost.goauthentik.io/auth/nginx;"
  rollbackNginx.locations."/".extraConfig;
assert rollbackNginx.locations."/opds".proxyPass == "http://127.0.0.1:8003";
assert lib.hasInfix "auth_request off;" rollbackNginx.locations."/opds".extraConfig;
assert koboNginx.locations."/".proxyPass == "http://127.0.0.1:8001/";
assert !lib.hasInfix "auth_request" koboNginx.locations."/".extraConfig;
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
    grep -Fxq security "$TMPDIR/modules"
    grep -Fxq http.handlers.authenticator "$TMPDIR/modules"
    grep -Fxq http.authentication.providers.authorizer "$TMPDIR/modules"
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

    touch "$out"
  ''
