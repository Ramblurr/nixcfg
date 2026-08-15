{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.services.calibre-web;
  mediaLocalPath = "/mnt/mali/${cfg.mediaNfsShare}";
  upstream = "http://127.0.0.1:${toString cfg.ports.http}";
in
{
  options.modules.services.calibre-web = {
    enable = lib.mkEnableOption "calibre-web";
    domain = lib.mkOption {
      type = lib.types.str;
      description = "The domain to use for the calibre-web";
    };

    domainKobo = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "The domain to use for the kobo endpoint";
    };
    ports = {
      http = lib.mkOption { type = lib.types.port; };
    };
    mediaNfsShare = lib.mkOption { type = lib.types.str; };
    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption { type = lib.types.unspecified; };
  };
  config = lib.mkIf cfg.enable {
    users.users.${cfg.user.name} = lib.mkForce {
      inherit (cfg.user) name;
      uid = lib.mkForce cfg.user.uid;
      isSystemUser = true;
      group = lib.mkForce cfg.group.name;
      extraGroups = lib.mkForce [ "media" ];
    };

    users.groups.${cfg.group.name} = {
      inherit (cfg.group) name;
      gid = lib.mkForce cfg.group.gid;
    };

    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/calibre-web"."mountpoint" = "/var/lib/calibre-web";
    };

    systemd.services.calibre-web.unitConfig = {
      RequiresMountsFor = [
        mediaLocalPath
        "/var/lib/calibre-web"
      ];
    };
    systemd.services.calibre-web.serviceConfig = {
      SupplementaryGroups = [ "media" ];
      LockPersonality = true;
      NoNewPrivileges = true;
      PrivateDevices = true;
      PrivateMounts = true;
      PrivateTmp = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      ProtectControlGroups = true;
      ProtectClock = true;
      ProtectProc = "invisible";
      ProtectHostname = true;
      ProtectKernelLogs = true;
      ProtectKernelModules = true;
      ProtectKernelTunables = true;
      ProcSubset = "pid";
      RemoveIPC = true;
      RestrictNamespaces = true;
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
      #SystemCallArchitectures = "native";
      #SystemCallFilter = [
      #  "@system-service"
      #  "~@privileged"
      #  "~@resources"
      #];
      CapabilityBoundingSet = "";
      AmbientCapabilities = "";
      MemoryDenyWriteExecute = true;
      UMask = "0002";
      RestrictAddressFamilies = [
        "AF_UNIX"
        "AF_INET"
        "AF_INET6"
        "AF_NETLINK"
      ];
      ReadWritePaths = [ "${mediaLocalPath}/books" ];
    };

    systemd.services.calibre-web.serviceConfig = {
      CacheDirectory = "calibre-web";
      Environment = [ "CACHE_DIR=/var/cache/calibre-web" ];
    };
    services.calibre-web = {
      enable = true;
      package = pkgs.calibre-web.overrideAttrs (prev: {
        propagatedBuildInputs = prev.propagatedBuildInputs ++ prev.passthru.optional-dependencies.kobo;
      });

      listen.port = cfg.ports.http;
      listen.ip = "127.0.0.1";
      options = {
        calibreLibrary = "${mediaLocalPath}/books";
        enableBookConversion = true;
        enableBookUploading = true;
        enableKepubify = true;
        reverseProxyAuth = {
          enable = true;
          header = "Remote-User";
        };
      };
    };

    modules.services.caddy.protectedRoutes.calibre-web = {
      publicHost = cfg.domain;
      inherit upstream;
      clientID = config.repo.secrets.home-ops.calibreWebPocketIdClientId;
      oidcRealm = "calibre-pocket-id";
      requiredGroups = [ "books" ];
      bypassPathPrefixes = [ "/opds" ];
      identityHeaders = {
        Remote-User = "userinfo|preferred_username";
        Remote-Name = "userinfo|preferred_username";
        Remote-Email = "email";
        Remote-Groups = "roles";
        X-Auth-Request-User = "sub";
        X-Auth-Request-Preferred-Username = "userinfo|preferred_username";
        X-Auth-Request-Email = "email";
        X-Auth-Request-Groups = "roles";
      };
    };
    modules.services.caddy.routes.books-kobo = lib.mkIf (cfg.domainKobo != "") {
      publicHost = cfg.domainKobo;
      inherit upstream;
      requestHeaders.X-Scheme = "https";
    };

  };
}
