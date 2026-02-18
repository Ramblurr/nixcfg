{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.services.my-y2r;
  inherit (config.repo.secrets) home-ops;
  dataDir = "/var/lib/y2r";
  inherit (config.modules.services.y2r) user;
  inherit (config.modules.services.y2r) group;
in
{
  options.modules.services.my-y2r = {
    enable = lib.mkEnableOption "my y2r";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "podcasts.example.com";
      description = "The domain to use for the podcast feed";
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

  config = lib.mkIf cfg.enable {
    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [ cfg.domain ];
      };
    };

    services.nginx.virtualHosts."${cfg.domain}" = {
      locations."~ /\\." = {
        extraConfig = "deny all;";
      };

      locations."~ \\.log$" = {
        extraConfig = "deny all;";
      };

      locations."~ ^/[^/]+/(inbox|processing|archive)(/|$)" = {
        extraConfig = "deny all;";
      };

      locations."~ \\.rss$" = {
        extraConfig = ''
          sub_filter 'http://${cfg.domain}/' 'https://${cfg.domain}/';
          sub_filter_once off;
          sub_filter_types application/rss+xml text/xml;
        '';
      };
    };
    modules.services.ingress.virtualHosts.${cfg.domain} = {
      acmeHost = cfg.ingress.domain;
      root = dataDir;
      extraConfig = ''
        types {
          application/rss+xml rss xml;
          audio/mpeg mp3;
          audio/mp4 m4a;
          video/mp4 mp4;
        }
        add_header Accept-Ranges bytes;
      '';
      upstreamExtraConfig = ''
        autoindex on;
        charset utf-8;
      '';
    };

    modules.zfs.datasets.properties = {
      "tank/svc/y2r"."mountpoint" = dataDir;
    };

    systemd.tmpfiles.rules = [
      "z '${dataDir}' 750 ${user} ${group} - -"
    ];
    users.users.nginx.extraGroups = [ "y2r" ];

    modules.services.y2r = {
      enable = true;
      settings = {
        host = cfg.domain;
        documentRoot = dataDir;
        defaultNumDownloads = 5;
        dateAfter = "2020-12-01";
      };
      feeds = home-ops.y2r-feeds;
    };
  };
}
