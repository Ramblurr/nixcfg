{
  options,
  config,
  lib,
  pkgs,
  inputs,
  unstable,
  ...
}:
let
  cfg = config.modules.services.davis;
  home-ops = config.repo.secrets.home-ops;
in
{
  options.modules.services.davis = {
    enable = lib.mkEnableOption "davis";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "dav.example.com";
      description = "The domain to use for the davis";
    };
    ingress = {
      external = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether to expose the service externally";
      };
      domain = lib.mkOption {
        type = lib.types.str;
        example = "example.com";
        description = "The ingress domain to use";
      };
    };
  };

  imports = [ "${inputs.nixpkgs-unstable}/nixos/modules/services/web-apps/davis.nix" ];
  config = lib.mkIf cfg.enable {
    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [ cfg.domain ];
      };
    };
    sops.secrets."davis/APP_SECRET" = {
      owner = config.services.davis.user;
      group = config.services.davis.group;
      mode = "400";
    };
    sops.secrets."davis/ADMIN_PASSWORD" = {
      owner = config.services.davis.user;
      group = config.services.davis.group;
      mode = "400";
    };
    services.davis = {
      enable = true;
      package = unstable.davis;
      hostname = cfg.domain;
      database = {
        driver = "postgresql";
      };
      mail = {
        dsn = home-ops.mail.dsn;
        inviteFromAddress = home-ops.mail.notificationsFromAddress;
      };
      adminLogin = "admin";
      adminPasswordFile = config.sops.secrets."davis/ADMIN_PASSWORD".path;
      appSecretFile = config.sops.secrets."davis/APP_SECRET".path;
      config = {
        IMAP_AUTH_URL = home-ops.mail.imapAuthUrl;
        AUTH_METHOD = "IMAP";
        IMAP_AUTH_USER_AUTOCREATE = false;
      };
      nginx = {
        useACMEHost = cfg.ingress.domain;
        onlySSL = true;
        kTLS = true;
      };
    };
  };
}
