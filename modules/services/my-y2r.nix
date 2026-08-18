{
  config,
  lib,
  pkgs,
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
  };

  config = lib.mkIf cfg.enable {

    modules.services.caddy.routes.y2pod = {
      publicHost = cfg.domain;
      handlerConfig = ''
        @y2pod_private path_regexp y2pod_private (^|/)\\.
        respond @y2pod_private 403
        @y2pod_logs path_regexp y2pod_logs \\.log$
        respond @y2pod_logs 403
        @y2pod_work path_regexp y2pod_work ^/[^/]+/(inbox|processing|archive)(/|$)
        respond @y2pod_work 403
        @y2pod_rss path_regexp y2pod_rss \\.rss$
        header @y2pod_rss Content-Type "application/rss+xml; charset=utf-8"
        header Accept-Ranges bytes
        root * ${dataDir}
        file_server browse
      '';
    };

    modules.zfs.datasets.properties = {
      "tank/svc/y2r"."mountpoint" = dataDir;
    };

    systemd.tmpfiles.rules = [
      "z '${dataDir}' 750 ${user} ${group} - -"
    ];
    users.users.caddy.extraGroups = [ group ];

    systemd.services = {
      caddy = {
        requires = [ "y2r-https-enclosures.service" ];
        after = [ "y2r-https-enclosures.service" ];
      };
      y2r-https-enclosures = {
        description = "Make existing y2pod enclosure URLs use HTTPS";
        wantedBy = [ "multi-user.target" ];
        unitConfig.RequiresMountsFor = [ dataDir ];
        serviceConfig = {
          Type = "oneshot";
          User = user;
          Group = group;
        };
        script = ''
          ${pkgs.findutils}/bin/find ${dataDir} -type f \( \
            -name feed.body -o -name feed.rss \) \
            -exec ${pkgs.gnused}/bin/sed -i \
            's#http://${cfg.domain}/#https://${cfg.domain}/#g' {} +
        '';
      };
    };

    modules.services.y2r = {
      enable = true;
      package = pkgs.youtube-to-rss.overrideAttrs (old: {
        patches = (old.patches or [ ]) ++ [ ./my-y2r-https-enclosures.patch ];
      });
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
