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
    appName = lib.mkOption {
      type = lib.types.nonEmptyStr;
      description = "Display name of the Forgejo instance";
    };
    domain = lib.mkOption {
      type = lib.types.str;
      example = "code.example.com";
      description = "The domain to use for the forgejo";
    };
    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption { type = lib.types.unspecified; };
  };

  config = lib.mkIf cfg.enable {
    users.users.forgejo.uid = lib.mkForce cfg.user.uid;
    users.groups.forgejo.gid = lib.mkForce cfg.group.gid;

    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/forgejo"."mountpoint" = config.services.forgejo.stateDir;
      "rpool/encrypted/safe/svc/forgejo"."com.sun:auto-snapshot" = "false";
    };
    services.forgejo = {
      enable = true;
      database.type = "sqlite3";
      settings = {
        DEFAULT.APP_NAME = cfg.appName;
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
          "explore.DISABLE_USERS_PAGE" = false;
        };
        api.ENABLE_SWAGGER = true;
        oauth2.ACCESS_TOKEN_EXPIRATION_TIME = 86400;
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

    # Forgejo keeps user and deploy keys in its own file. Our sshd reads only
    # /etc/ssh/authorized_keys.d/%u, so point it at that file for this user.
    services.openssh.extraConfig = ''
      Match User ${config.services.forgejo.user}
        AuthorizedKeysFile ${config.services.forgejo.stateDir}/.ssh/authorized_keys
    '';

    site.gatus.endpoints = [
      {
        name = "Forgejo (${cfg.domain})";
        group = config.site.gatus.groups.work;
        url = "https://${cfg.domain}/";
      }
    ];

    modules.services.caddy.routes.forgejo = {
      publicHost = cfg.domain;
      upstream = "unix/${config.services.forgejo.settings.server.HTTP_ADDR}";
      requestBodyMaxSize = "1GB";
    };
  };
}
