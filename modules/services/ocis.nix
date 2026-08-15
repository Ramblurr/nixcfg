{
  config,
  lib,
  utils,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.services.ocis;
  localPath = "/mnt/mali/${cfg.nfsShare}";
  serviceDeps = [ "${utils.escapeSystemdPath localPath}.mount" ];
  cspFile = pkgs.writeText "csp.yaml" cfg.cspYaml;
in
{
  disabledModules = [
    "${inputs.nixpkgs}/nixos/modules/services/web-apps/ocis.nix"
    "${inputs.nixpkgs-stable}/nixos/modules/services/web-apps/ocis.nix"
  ];
  imports = [
    "${inputs.nixpkgs-mine}/nixos/modules/services/web-apps/ocis.nix"
  ];
  options.modules.services.ocis = {
    enable = lib.mkEnableOption "ocis";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "ocis.example.com";
      description = "The domain to use for the ocis";
    };

    cspYaml = lib.mkOption {
      type = lib.types.str;
      description = ''
        A CSP yaml file (see https://doc.owncloud.com/ocis/next/deployment/services/s-list/proxy.html#content-security-policy)
      '';
    };

    oidc = {
      issuer = lib.mkOption {
        type = lib.types.str;
        description = "OpenID Connect issuer URL.";
      };
      clientId = lib.mkOption {
        type = lib.types.str;
        description = "Public oCIS web client ID.";
      };
      scopes = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [
          "openid"
          "profile"
          "email"
          "groups"
        ];
        description = "OpenID Connect scopes requested by the oCIS web client.";
      };
      autoProvisionAccounts = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether oCIS provisions and reconciles users and groups from OIDC claims.";
      };
      userOidcClaim = lib.mkOption {
        type = lib.types.str;
        default = "preferred_username";
        description = "OIDC claim used to identify an oCIS user.";
      };
      userCs3Claim = lib.mkOption {
        type = lib.types.enum [
          "username"
          "mail"
          "userid"
        ];
        default = "username";
        description = "oCIS user attribute matched against the OIDC identity claim.";
      };
      roleAssignmentDriver = lib.mkOption {
        type = lib.types.enum [
          "default"
          "oidc"
        ];
        default = "default";
        description = "oCIS role assignment mechanism.";
      };
      rewriteWellKnown = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether oCIS rewrites discovery for native clients.";
      };
      accessTokenVerifyMethod = lib.mkOption {
        type = lib.types.enum [
          "jwt"
          "none"
        ];
        default = "jwt";
        description = "Access token verification method used by the oCIS proxy.";
      };
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
    subnet = lib.mkOption { type = lib.types.unspecified; };
  };

  config = lib.mkIf cfg.enable {

    users.users.${cfg.user.name} = {
      inherit (cfg.user) name;
      uid = lib.mkForce cfg.user.uid;
      isSystemUser = true;
      group = lib.mkForce cfg.group.name;
    };

    users.groups.${cfg.group.name} = {
      inherit (cfg.group) name;
      gid = lib.mkForce cfg.group.gid;
    };

    fileSystems."${localPath}" = {
      device = "${lib.my.cidrToIp config.repo.secrets.global.nodes.mali.dataCIDR}:/mnt/${cfg.nfsShare}";
      fsType = "nfs";
    };

    modules.networking.systemd-netns-private = {
      enable = true;
      namespaces.ocis = {
        inherit (cfg.subnet) hostAddr;
        inherit (cfg.subnet) nsAddr;
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
      #package = pkgs.ocis_71-bin;
      package = pkgs.ocis_72-bin;
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
        PROXY_CSP_CONFIG_FILE_LOCATION = "${cspFile}";
        NOTIFICATIONS_SMTP_HOST = toString config.repo.secrets.home-ops.mail.host;
        NOTIFICATIONS_SMTP_PORT = toString config.repo.secrets.home-ops.mail.port;
        NOTIFICATIONS_SMTP_SENDER = config.repo.secrets.home-ops.mail.notificationsFromAddressWork;
        NOTIFICATIONS_SMTP_INSECURE = "true";
        #OCIS_LOG_LEVEL = "debug";
        OCIS_LOG_COLOR = "true";
        OCIS_LOG_PRETTY = "true";
        OCIS_OIDC_ISSUER = cfg.oidc.issuer;
        WEB_OIDC_CLIENT_ID = cfg.oidc.clientId;
        PROXY_OIDC_ACCESS_TOKEN_VERIFY_METHOD = cfg.oidc.accessTokenVerifyMethod;
        PROXY_OIDC_REWRITE_WELLKNOWN = lib.boolToString cfg.oidc.rewriteWellKnown;
        PROXY_AUTOPROVISION_ACCOUNTS = lib.boolToString cfg.oidc.autoProvisionAccounts;
        PROXY_USER_OIDC_CLAIM = cfg.oidc.userOidcClaim;
        PROXY_USER_CS3_CLAIM = cfg.oidc.userCs3Claim;
        PROXY_ROLE_ASSIGNMENT_DRIVER = cfg.oidc.roleAssignmentDriver;
        WEB_OIDC_SCOPE = lib.concatStringsSep " " cfg.oidc.scopes;
        OCIS_SHARING_PUBLIC_SHARE_MUST_HAVE_PASSWORD = "false";
      };
    };

    modules.services.caddy.routes.data = {
      publicHost = cfg.domain;
      upstream = "http://${lib.my.cidrToIp cfg.subnet.nsAddr}:${toString cfg.ports.http}";
    };
  };
}
