{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.services.forgejo;
in
#httpPort = toString cfg.ports.http;
{
  options.modules.services.forgejo = {
    enable = lib.mkEnableOption "forgejo";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "code.example.com";
      description = "The domain to use for the forgejo";
    };
    #ports = {
    #  http = lib.mkOption {
    #    type = lib.types.port;
    #    description = "The HTTP port to use for the forgejo";
    #  };
    #};
    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
    };
  };

  config = lib.mkIf cfg.enable {
    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/forgejo"."mountpoint" = config.services.forgejo.stateDir;
      "rpool/encrypted/safe/svc/forgejo"."com.sun:auto-snapshot" = "false";
    };
    services.forgejo = {
      enable = true;
      database.type = "sqlite3";
      settings = {
        DEFAULT.APP_NAME = "Cozy Code Cabin";
        log.LEVEL = "Warn";
        database.LOG_SQL = false;
        server = {
          DOMAIN = config.modules.services.forgejo.domain;
          ROOT_URL = "https://${config.modules.services.forgejo.domain}/";
          PROTOCOL = "http+unix";
          OFFLINE_MODE = true; # disable use of CDNs
        };
        service = {
          DISABLE_REGISTRATION = true;
          ENABLE_NOTIFY_MAIL = false;
          "explore.DISABLE_USERS_PAGE" = true;
        };
        openid = {
          ENABLE_OPENID_SIGNIN = false;
          ENABLE_OPENID_SIGNUP = false;
        };
        mailer = {
          ENABLED = false;
        };
        session = {
          COOKIE_SECURE = true;
          PROVIDER = "db";
        };
        actions.ENABLED = true;
      };
    };

    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [ cfg.domain ];
      };
    };
    modules.services.ingress.virtualHosts.${cfg.domain} = {
      acmeHost = cfg.ingress.domain;
      upstream = "http://unix:${config.services.forgejo.settings.server.HTTP_ADDR}:/";
      #forwardAuth = true;
    };
  };
}
