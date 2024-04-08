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
  cfg = config.modules.services.ocis;
  home-ops = config.repo.secrets.home-ops;
  localPath = "/mnt/mali/${cfg.nfsShare}";
  serviceDeps = [ "${utils.escapeSystemdPath localPath}.mount" ];
in
{
  options.modules.services.ocis = {
    enable = lib.mkEnableOption "ocis";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "ocis.example.com";
      description = "The domain to use for the ocis";
    };

    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
    };
    ports = {
      http = lib.mkOption {
        type = lib.types.port;
        description = "The HTTP port to use for ocis";
      };
    };
    nfsShare = lib.mkOption { type = lib.types.str; };
    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption { type = lib.types.unspecified; };
  };

  disabledModules = [
    "${inputs.nixpkgs-stable}/nixos/modules/services/web-apps/ocis.nix"
    "${inputs.nixpkgs-unstable}/nixos/modules/services/web-apps/ocis.nix"
  ];
  imports = [ "${inputs.nixpkgs-mine}/nixos/modules/services/web-apps/ocis.nix" ];

  config = lib.mkIf cfg.enable {

    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [ cfg.domain ];
      };
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

    modules.networking.systemd-netns-private = {
      enable = true;
      namespaces.ocis = {
        hostAddr = "192.168.10.1/24";
        nsAddr = "192.168.10.2/24";
        services = [ "ocis.service" ];
      };
    };

    systemd.services.ocis.after = serviceDeps;
    systemd.services.ocis.bindsTo = serviceDeps;
    systemd.services.ocis.serviceConfig = {
      ReadWritePaths = [ "/tmp" ];
    };

    services.ocis = {
      enable = true;
      package = unstable.ocis-bin;
      url = "https://${cfg.domain}";
      stateDir = "${localPath}/data";
      configDir = "${localPath}/config";
      user = cfg.user.name;
      group = cfg.group.name;
      port = cfg.ports.http;
      address = "0.0.0.0";
      environment = {
        OCIS_INSECURE = "true";
        PROXY_TLS = "false";
        NOTIFICATIONS_SMTP_HOST = toString config.repo.secrets.home-ops.mail.host;
        NOTIFICATIONS_SMTP_PORT = toString config.repo.secrets.home-ops.mail.port;
        NOTIFICATIONS_SMTP_SENDER = config.repo.secrets.home-ops.mail.notificationsFromAddressWork;
        NOTIFICATIONS_SMTP_INSECURE = "true";
        OCIS_LOG_LEVEL = "debug";
        OCIS_LOG_COLOR = "true";
        OCIS_LOG_PRETTY = "true";
        # Authentik OIDC
        OCIS_OIDC_ISSUER = "https://auth.${config.repo.secrets.home-ops.workDomain}/application/o/work-ocis/";
        WEB_OIDC_CLIENT_ID = "work-ocis";
        # Without this, I got the following errors in the ownCloud log:
        # Authelia: failed to verify access token: token contains an invalid number of segments
        # Authentik:  failed to verify access token: the JWT has an invalid kid: could not find kid in JWT header
        PROXY_OIDC_ACCESS_TOKEN_VERIFY_METHOD = "none";
        PROXY_OIDC_REWRITE_WELLKNOWN = "true";
        PROXY_AUTOPROVISION_ACCOUNTS = "true";
        # Auto role assignment
        # docs: https://doc.owncloud.com/ocis/next/deployment/services/s-list/proxy.html#automatic-role-assignments
        PROXY_USER_OIDC_CLAIM = "preferred_username";
        PROXY_ROLE_ASSIGNMENT_DRIVER = "oidc";
        WEB_OIDC_SCOPE = "openid profile email groups";
      };
    };

    services.nginx.virtualHosts.${cfg.domain} = {
      useACMEHost = cfg.ingress.domain;
      forceSSL = true;
      kTLS = true;
      extraConfig = ''
        client_max_body_size 0;
        client_header_buffer_size 64k;
      '';
      locations."/" = {
        proxyPass = "http://192.168.10.2:${toString cfg.ports.http}";
        recommendedProxySettings = true;
      };
    };
  };
}
