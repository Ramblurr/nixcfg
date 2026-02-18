{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.services.y2r;

  # Generate app.yaml content
  appYaml = pkgs.writeText "app.yaml" (
    builtins.toJSON {
      site = {
        inherit (cfg.settings) host;
        document_root = cfg.settings.documentRoot;
      };
      defaults = {
        numdl = cfg.settings.defaultNumDownloads;
        dateafter = cfg.settings.dateAfter;
      };
      yt_dlp = {
        executable = lib.getExe cfg.ytDlpPackage;
        common_args = cfg.settings.ytDlpArgs;
        extractor_args = cfg.settings.ytExtractorArgs;
      };
      auth = lib.optionalAttrs (cfg.settings.cookiesFile != null) {
        cookies_file = cfg.settings.cookiesFile;
        use_cookies_on_fail = cfg.settings.useCookiesOnFail;
        fail_signatures = cfg.settings.failSignatures;
      };
    }
  );

  # Generate feeds.yaml content
  feedsYaml = pkgs.writeText "feeds.yaml" (
    builtins.toJSON {
      feeds = lib.mapAttrsToList (name: feed: {
        id = name;
        inherit (feed) title channelurl;
        thumburl = feed.thumbUrl;
      }) cfg.feeds;
    }
  );

  # The package
  y2rPkg = cfg.package;
in
{
  options.modules.services.y2r = {
    enable = lib.mkEnableOption "YouTube to RSS podcast feed generator";

    package = lib.mkPackageOption pkgs "youtube-to-rss" { };

    ytDlpPackage = lib.mkPackageOption pkgs "yt-dlp" { };

    ffmpegPackage = lib.mkPackageOption pkgs "ffmpeg" {
      default = "ffmpeg-headless";
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "y2r";
      description = "User account under which y2r runs.";
    };

    group = lib.mkOption {
      type = lib.types.str;
      default = "y2r";
      description = "Group under which y2r runs.";
    };

    settings = {
      host = lib.mkOption {
        type = lib.types.str;
        example = "podcasts.example.com";
        description = "Hostname where the RSS feeds will be served.";
      };

      documentRoot = lib.mkOption {
        type = lib.types.path;
        default = "/var/lib/y2r/feeds";
        description = "Base directory where feed files are stored and served.";
      };

      defaultNumDownloads = lib.mkOption {
        type = lib.types.int;
        default = 3;
        description = "Default number of recent videos to download per run.";
      };

      dateAfter = lib.mkOption {
        type = lib.types.str;
        default = "2024-01-01";
        example = "2024-06-15";
        description = "Only download videos published after this date (YYYY-MM-DD).";
      };

      ytDlpArgs = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [
          "--download-archive"
          "archive"
          "--write-info-json"
          "--no-simulate"
          "-f"
          "bestaudio[ext=m4a]/bestaudio/best"
        ];
        description = "Common arguments passed to yt-dlp.";
      };

      ytExtractorArgs = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "youtube:player_client=web";
        description = "YouTube extractor arguments for yt-dlp.";
      };

      cookiesFile = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        description = "Path to YouTube cookies file for authenticated downloads.";
      };

      useCookiesOnFail = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Retry with cookies when authentication-related errors occur.";
      };

      failSignatures = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [
          "sign in"
          "age-restricted"
          "private video"
        ];
        description = "Error message patterns that trigger cookie retry.";
      };
    };

    feeds = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule {
          options = {
            title = lib.mkOption {
              type = lib.types.str;
              description = "Display title for the feed.";
            };

            channelurl = lib.mkOption {
              type = lib.types.str;
              description = "YouTube channel or playlist URL.";
            };

            thumbUrl = lib.mkOption {
              type = lib.types.str;
              default = "";
              description = "URL for the feed thumbnail image.";
            };

            schedule = lib.mkOption {
              type = lib.types.str;
              default = "*-*-* 00,04,08,12,16,20:00:00";
              example = "hourly";
              description = "Systemd calendar expression for download schedule.";
            };

            maxDownloads = lib.mkOption {
              type = lib.types.nullOr lib.types.int;
              default = null;
              description = "Override default number of downloads for this feed.";
            };
          };
        }
      );
      default = { };
      description = "Attribute set of feed configurations, keyed by feed ID.";
    };
  };

  config = lib.mkIf cfg.enable {
    users.users = lib.mkIf (cfg.user == "y2r") {
      y2r = {
        isSystemUser = true;
        inherit (cfg) group;
        home = cfg.settings.documentRoot;
      };
    };

    users.groups = lib.mkIf (cfg.group == "y2r") { y2r = { }; };

    systemd.tmpfiles.rules = [
      "d ${cfg.settings.documentRoot} 0755 ${cfg.user} ${cfg.group} -"
    ];

    # Create download services and RSS update services for each feed
    systemd.services =
      # Download services
      lib.mapAttrs' (
        name: feed:
        lib.nameValuePair "y2r-download-${name}" {
          description = "Download YouTube videos for feed: ${name}";
          after = [ "network-online.target" ];
          wants = [ "network-online.target" ];

          path = [ cfg.ffmpegPackage ];
          serviceConfig = {
            Type = "oneshot";
            User = cfg.user;
            Group = cfg.group;
            ExecStart =
              let
                numArg = if feed.maxDownloads != null then "-n ${toString feed.maxDownloads}" else "";
              in
              "${y2rPkg}/bin/y2r download --app ${appYaml} --feeds ${feedsYaml} ${name} ${numArg}";
            StandardOutput = "journal";
            StandardError = "journal";
          };
        }
      ) cfg.feeds
      # RSS update services
      // lib.mapAttrs' (
        name: _feed:
        lib.nameValuePair "y2r-update-${name}" {
          description = "Update RSS feed: ${name}";

          path = [ cfg.ffmpegPackage ];
          serviceConfig = {
            Type = "oneshot";
            User = cfg.user;
            Group = cfg.group;
            ExecStart = "${y2rPkg}/bin/y2r update-rss --app ${appYaml} --feeds ${feedsYaml} ${name}";
            StandardOutput = "journal";
            StandardError = "journal";
          };
        }
      ) cfg.feeds;

    systemd.timers = lib.mapAttrs' (
      name: feed:
      lib.nameValuePair "y2r-download-${name}" {
        description = "Timer for downloading YouTube videos for feed: ${name}";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = feed.schedule;
          Persistent = true;
          RandomizedDelaySec = "5min";
        };
      }
    ) cfg.feeds;

    # Create path watchers for RSS updates
    systemd.paths = lib.mapAttrs' (
      name: _feed:
      lib.nameValuePair "y2r-update-${name}" {
        description = "Watch for new files in inbox for feed: ${name}";
        wantedBy = [ "multi-user.target" ];
        pathConfig = {
          PathModified = "${cfg.settings.documentRoot}/${name}/inbox";
          Unit = "y2r-update-${name}.service";
        };
      }
    ) cfg.feeds;
  };
}
