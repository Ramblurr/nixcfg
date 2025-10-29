{
  options,
  config,
  lib,
  pkgs,
  inputs,
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

  disabledModules = [
    "${inputs.nixpkgs}/nixos/modules/services/web-apps/davis.nix"
    "${inputs.nixpkgs-stable}/nixos/modules/services/web-apps/davis.nix"
  ];
  imports = [
    "${inputs.nixpkgs-mine}/nixos/modules/services/web-apps/davis.nix"
  ];
  config = lib.mkIf cfg.enable {
    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [ cfg.domain ];
      };
    };
    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/davis"."mountpoint" = config.services.davis.dataDir;
      "rpool/encrypted/safe/svc/davis"."com.sun:auto-snapshot" = "false";
    };
    sops.secrets."davis/APP_SECRET" = {
      sopsFile = ../../configs/home-ops/shared.sops.yml;
      owner = config.services.davis.user;
      group = config.services.davis.group;
      mode = "400";
    };
    sops.secrets."davis/ADMIN_PASSWORD" = {
      sopsFile = ../../configs/home-ops/shared.sops.yml;
      owner = config.services.davis.user;
      group = config.services.davis.group;
      mode = "400";
    };

    services.davis = {
      enable = true;
      hostname = cfg.domain;
      package = pkgs.davis;
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
        IMAP_AUTH_URL = home-ops.mail.imapAuthUrlNew;
        IMAP_ENCRYPTION_METHOD = "ssl";
        IMAP_CERTIFICATE_VALIDATION = true;
        AUTH_METHOD = "IMAP";
        IMAP_AUTH_USER_AUTOCREATE = false;
      };
      nginx = {
        useACMEHost = cfg.ingress.domain;
        forceSSL = true;
        kTLS = true;
        http3 = true;
        http2 = false;
        quic = true;
        locations."/" = {
          extraConfig = ''
            add_header Alt-Svc 'h3=":443"; ma=86400';
          '';
        };
      };
    };
  };
}
