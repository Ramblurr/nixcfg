{
  inputs,
  pkgs,
}:
let
  inherit (pkgs) lib;
  evaluated = inputs.nixpkgs.lib.nixosSystem {
    modules = [
      inputs.impermanence.nixosModules.impermanence
      inputs.sops-nix.nixosModules.sops
      ../modules/services/oauth2-proxy.nix
      ../modules/services/ingress.nix
      ({ lib, ... }: {
        options.modules.services.authentik.ports.http = lib.mkOption {
          type = lib.types.port;
          default = 9000;
        };
      })
      {
        nixpkgs.pkgs = pkgs;
        system.stateVersion = "26.05";

        services.nginx.virtualHosts."id.example.test".locations."/".proxyPass = "http://127.0.0.1:1411";

        modules.services = {
          oauth2-proxy = {
            enable = true;
            issuerURL = "https://id.example.test";
            publicHost = "id.example.test";
            cookieDomain = ".example.test";
            clientID = "oauth2-proxy";
            secretEnvironmentFile = "/run/secrets/oauth2-proxy.env";
          };
          ingress = {
            enable = true;
            oauth2Proxy.host = "id.example.test";
            virtualHosts = {
              "old.example.test" = {
                acmeHost = "example.test";
                upstream = "http://127.0.0.1:8000";
                forwardAuth = true;
              };
              "books.example.test" = {
                acmeHost = "example.test";
                upstream = "http://127.0.0.1:8083";
                forwardAuth = true;
                usesPocketId = true;
                forwardAuthGroups = [ "books" ];
              };
            };
          };
        };
      }
    ];
  };

  cfg = evaluated.config;
  proxy = cfg.services.oauth2-proxy;
  service = cfg.systemd.services.oauth2-proxy;
  virtualHost = cfg.services.nginx.virtualHosts."id.example.test";
  ingress = cfg.modules.services.ingress;
  oldHost = cfg.services.nginx.virtualHosts."old.example.test";
  booksHost = cfg.services.nginx.virtualHosts."books.example.test";
  nginxExecStart = cfg.systemd.services.nginx.serviceConfig.ExecStart;
in
assert pkgs.oauth2-proxy.version == "7.15.3";
assert proxy.enable;
assert proxy.provider == "oidc";
assert proxy.httpAddress == "http://127.0.0.1:4180";
assert proxy.oidcIssuerUrl == "https://id.example.test";
assert proxy.redirectURL == "https://id.example.test/oauth2/callback";
assert proxy.proxyPrefix == "/oauth2";
assert proxy.scope == "openid email profile groups";
assert proxy.upstream == [ "static://202" ];
assert proxy.reverseProxy;
assert proxy.setXauthrequest;
assert proxy.keyFile == "/run/secrets/oauth2-proxy.env";
assert proxy.cookie.domain == ".example.test";
assert proxy.cookie.secure;
assert proxy.cookie.httpOnly;
assert proxy.extraConfig."code-challenge-method" == "S256";
assert proxy.extraConfig."cookie-path" == "/";
assert proxy.extraConfig."cookie-samesite" == "lax";
assert proxy.extraConfig."skip-provider-button";
assert
  proxy.trustedProxyIP == [
    "127.0.0.1/32"
    "::1/128"
  ];
assert proxy.extraConfig."whitelist-domain" == [ ".example.test" ];
assert service.serviceConfig.EnvironmentFile == "/run/secrets/oauth2-proxy.env";
assert virtualHost.locations."/".proxyPass == "http://127.0.0.1:1411";
assert virtualHost.locations."/oauth2/".proxyPass == "http://127.0.0.1:4180";
assert virtualHost.locations."= /oauth2".return == "302 /oauth2/";
assert !(builtins.elem 4180 cfg.networking.firewall.allowedTCPPorts);
assert !ingress.virtualHosts."old.example.test".usesPocketId;
assert ingress.virtualHosts."books.example.test".forwardAuthGroups == [ "books" ];
assert lib.hasInfix "auth_request        /outpost.goauthentik.io/auth/nginx;"
  oldHost.locations."/".extraConfig;
assert lib.hasInfix "auth_request /_oauth2_proxy_auth;" booksHost.locations."/".extraConfig;
assert lib.hasInfix "https://id.example.test/oauth2/auth?allowed_groups=books"
  booksHost.locations."= /_oauth2_proxy_auth".extraConfig;
assert lib.hasInfix "proxy_set_header X-Forwarded-Host $host;"
  booksHost.locations."= /_oauth2_proxy_auth".extraConfig;
assert lib.hasInfix "proxy_set_header X-Forwarded-Uri $request_uri;"
  booksHost.locations."= /_oauth2_proxy_auth".extraConfig;
assert lib.hasInfix "proxy_set_header X-Auth-Request-Redirect $scheme://$host$request_uri;"
  booksHost.locations."= /_oauth2_proxy_auth".extraConfig;
assert lib.hasInfix "X-authentik-username $oauth2_preferred_username"
  booksHost.locations."/".extraConfig;
assert lib.hasInfix "X-authentik-groups $oauth2_groups" booksHost.locations."/".extraConfig;
assert lib.hasInfix "return 302 https://id.example.test/oauth2/start"
  booksHost.locations."@oauth2_proxy_signin".extraConfig;
pkgs.runCommand "oauth2-proxy-module-test" { } ''
  printf '%s\n' ${lib.escapeShellArg nginxExecStart} > "$TMPDIR/nginx-exec-start"
  touch "$out"
''
