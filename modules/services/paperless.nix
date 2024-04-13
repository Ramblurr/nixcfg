{
  options,
  config,
  lib,
  utils,
  pkgs,
  inputs,
  unstable,
  mine,
  ...
}:
let
  cfg = config.modules.services.paperless;
  home-ops = config.repo.secrets.home-ops;
  localPath = "/mnt/mali/${cfg.nfsShare}";

  serviceDeps = [ "${utils.escapeSystemdPath localPath}.mount" ];
in
{
  options.modules.services.paperless = {
    enable = lib.mkEnableOption "paperless";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "paperless.example.com";
      description = "The domain to use for the paperless";
    };

    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
    };
    ports = {
      http = lib.mkOption {
        type = lib.types.port;
        description = "The HTTP port to use for paperless";
      };
    };
    nfsShare = lib.mkOption { type = lib.types.str; };
    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption { type = lib.types.unspecified; };
  };

  disabledModules = [
    "${inputs.nixpkgs-stable}/nixos/modules/services/misc/paperless.nix"
    "${inputs.nixpkgs-unstable}/nixos/modules/services/misc/paperless.nix"
  ];
  imports = [ "${inputs.nixpkgs-mine}/nixos/modules/services/misc/paperless.nix" ];

  config = lib.mkIf cfg.enable {

    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [ cfg.domain ];
      };
    };

    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/paperless"."mountpoint" = config.services.paperless.dataDir;
      "rpool/encrypted/safe/svc/paperless"."com.sun:auto-snapshot" = "false";
    };

    sops.secrets."paperless/adminPassword" = {
      sopsFile = ../../configs/home-ops/shared.sops.yml;
      owner = config.services.paperless.user;
      mode = "400";
    };

    services.postgresql = {
      ensureDatabases = [ "paperless" ];
      ensureUsers = [
        {
          name = cfg.user.name;
          ensureDBOwnership = true;
        }
      ];
    };

    users.users.${cfg.user.name} = {
      name = cfg.user.name;
      uid = lib.mkForce cfg.user.uid;
      isSystemUser = true;
      group = lib.mkForce cfg.group.name;
    };

    users.groups.${cfg.group.name} = {
      name = cfg.group.name;
      gid = lib.mkForce cfg.group.gid;
    };

    fileSystems."${localPath}" = {
      device = "${config.repo.secrets.global.nodes.mali.data}:/mnt/${cfg.nfsShare}";
      fsType = "nfs";
    };

    systemd.services.paperless-scheduler.after = serviceDeps;
    systemd.services.paperless-copy-password.after = serviceDeps;
    systemd.services.paperless-consumer.after = serviceDeps;
    systemd.services.paperless-task-queue.after = serviceDeps;
    systemd.services.paperless-web.after = serviceDeps;
    systemd.services.paperless-scheduler.bindsTo = serviceDeps;
    systemd.services.paperless-copy-password.bindsTo = serviceDeps;
    systemd.services.paperless-consumer.bindsTo = serviceDeps;
    systemd.services.paperless-task-queue.bindsTo = serviceDeps;
    systemd.services.paperless-web.bindsTo = serviceDeps;

    systemd.tmpfiles.rules =
      let
        paperless = config.services.paperless;
      in
      [
        "d '${paperless.dataDir}' - ${paperless.user} ${config.users.users.${paperless.user}.group} - -"
      ];
    systemd.tmpfiles.settings."10-paperless" = lib.mkForce { };
    services.paperless = {
      enable = true;
      package = unstable.paperless-ngx;
      mediaDir = "${localPath}/media";
      consumptionDir = "${localPath}/consume";
      passwordFile = config.sops.secrets."paperless/adminPassword".path;
      port = cfg.ports.http;
      user = cfg.user.name;
      settings = {
        PAPERLESS_EXPORT_DIR = "${localPath}/export";
        PAPERLESS_DBENGINE = "postgresql";
        PAPERLESS_DBHOST = "/run/postgresql";
        PAPERLESS_DBNAME = "paperless";
        PAPERLESS_CONSUMER_POLLING = 60;
        PAPERLESS_CONSUMER_RECURSIVE = true;
        PAPERLESS_CONSUMER_SUBDIRS_AS_TAGS = true;
        PAPERLESS_OCR_LANGUAGE = "deu+eng";
        PAPERLESS_PORT = 8080;
        PAPERLESS_TASK_WORKERS = 2;
        PAPERLESS_TIKA_ENABLED = 0;
        PAPERLESS_TIKA_GOTENBERG_ENDPOINT = "http://localhost:3000";
        PAPERLESS_TIKA_ENDPOINT = "http://localhost:9998";
        PAPERLESS_TIME_ZONE = "Europe/Berlin";
        PAPERLESS_FILENAME_FORMAT = "{created_year}/{created_year}-{created_month}-{created_day} {title}";
        PAPERLESS_FILENAME_DATE_ORDER = "YMD";
        PAPERLESS_URL = "https://${cfg.domain}";
        PAPERLESS_OCR_MAX_IMAGE_PIXELS = 956000000;
        PAPERLESS_ENABLE_HTTP_REMOTE_USER = true;
        PAPERLESS_HTTP_REMOTE_USER_HEADER_NAME = "X-authentik-username";
        #PAPERLESS_APPS = "allauth.socialaccount.providers.openid_connect";
        PAPERLESS_ACCOUNT_ALLOW_SIGNUPS = "false";
      };
    };

    modules.services.ingress.virtualHosts.${cfg.domain} = {
      acmeHost = cfg.ingress.domain;
      upstream = "http://127.0.0.1:${toString cfg.ports.http}";
      forwardAuth = true;
      extraConfig = ''
        client_max_body_size 0;
      '';
    };
  };
}
