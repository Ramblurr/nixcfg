{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.services.oauth2-proxy;
in
{
  options.modules.services.oauth2-proxy = {
    enable = lib.mkEnableOption "Pocket ID authentication through oauth2-proxy";

    issuerURL = lib.mkOption {
      type = lib.types.str;
      description = "Pocket ID OIDC issuer URL";
    };

    publicHost = lib.mkOption {
      type = lib.types.str;
      description = "Existing nginx hostname that serves the /oauth2 endpoints";
    };

    cookieDomain = lib.mkOption {
      type = lib.types.str;
      description = "Parent domain shared by oauth2-proxy and protected applications";
    };

    clientID = lib.mkOption {
      type = lib.types.str;
      description = "Pocket ID OIDC client ID";
    };

    secretEnvironmentFile = lib.mkOption {
      type = lib.types.str;
      description = "Runtime path to the SOPS-rendered oauth2-proxy environment file";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = lib.hasPrefix "https://" cfg.issuerURL;
        message = "modules.services.oauth2-proxy.issuerURL must use HTTPS";
      }
      {
        assertion = lib.hasPrefix "." cfg.cookieDomain;
        message = "modules.services.oauth2-proxy.cookieDomain must begin with a dot";
      }
      {
        assertion =
          lib.hasPrefix "/" cfg.secretEnvironmentFile
          && !lib.hasPrefix "/nix/store/" cfg.secretEnvironmentFile;
        message = "oauth2-proxy secret environment file must use an absolute runtime path outside the Nix store";
      }
    ];

    services.oauth2-proxy = {
      enable = true;
      provider = "oidc";
      inherit (cfg) clientID;
      oidcIssuerUrl = cfg.issuerURL;
      redirectURL = "https://${cfg.publicHost}/oauth2/callback";
      scope = "openid email profile groups";
      email.domains = [ "*" ];
      httpAddress = "http://127.0.0.1:4180";
      proxyPrefix = "/oauth2";
      upstream = [ "static://202" ];
      reverseProxy = true;
      trustedProxyIP = [
        "127.0.0.1/32"
        "::1/128"
      ];
      setXauthrequest = true;
      keyFile = cfg.secretEnvironmentFile;
      cookie = {
        domain = cfg.cookieDomain;
        secure = true;
        httpOnly = true;
      };
      extraConfig = {
        code-challenge-method = "S256";
        cookie-path = "/";
        cookie-samesite = "lax";
        skip-provider-button = true;
        whitelist-domain = [ cfg.cookieDomain ];
      };
    };

    services.nginx = {
      enable = true;
      virtualHosts.${cfg.publicHost}.locations = {
        "= /oauth2".return = "302 /oauth2/";
        "/oauth2/" = {
          proxyPass = "http://127.0.0.1:4180";
          recommendedProxySettings = true;
          extraConfig = ''
            auth_request off;
            proxy_set_header X-Scheme $scheme;
            proxy_set_header X-Auth-Request-Redirect $scheme://$host$request_uri;
          '';
        };
      };
    };

    systemd.services.oauth2-proxy.serviceConfig = {
      LockPersonality = true;
      MemoryDenyWriteExecute = true;
      NoNewPrivileges = true;
      PrivateDevices = true;
      PrivateTmp = true;
      ProtectHome = true;
      ProtectSystem = "strict";
      RestrictAddressFamilies = [
        "AF_UNIX"
        "AF_INET"
        "AF_INET6"
      ];
      RestrictNamespaces = true;
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
    };
  };
}
