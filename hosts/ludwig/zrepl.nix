{
  config,
  lib,
  pkgs,
  ...
}:
#
# ludwig is mali's offsite replica: it pulls mali's mali_source job over TLS.
# Ported from the TrueNAS jail config (zrepl 0.6.1).
let
  inherit (config.repo.secrets.local) ludwig;
  inherit (import ./site.nix) snapshotPrefix;
  receiveRoot = "tank/replication/mali";
  # mali's own zrepl snapshot prefix.
  maliPrefix = "zrepl_";
  secret = name: config.sops.secrets."zrepl/${name}".path;
  snapshotAge = pkgs.callPackage ../../pkgs/zfs-snapshot-age.nix { zfs = config.boot.zfs.package; };
  keepNonPrefixed = prefix: {
    type = "regex";
    negate = true;
    regex = "^${prefix}";
  };
in
{
  sops.secrets."zrepl/ludwigCert" = { };
  sops.secrets."zrepl/ludwigKey" = { };
  sops.secrets."zrepl/maliCert" = { };

  services.zrepl = {
    enable = true;
    settings = {
      global = {
        logging = [
          {
            type = "syslog";
            level = "info";
            format = "human";
          }
        ];
        monitoring = [
          {
            type = "prometheus";
            listen = ":9811";
            listen_freebind = true;
          }
        ];
      };
      jobs = [
        {
          # Mali's replication cursors and step holds are keyed by this name.
          name = "pull_mali";
          type = "pull";
          connect = {
            type = "tls";
            address = ludwig.zreplMaliAddress;
            ca = secret "maliCert";
            cert = secret "ludwigCert";
            key = secret "ludwigKey";
            server_cn = "mali";
          };
          root_fs = receiveRoot;
          interval = "10m";
          recv = {
            properties.override = {
              mountpoint = "none";
              canmount = "off";
              readonly = "on";
            };
            placeholder.encryption = "inherit";
          };
          pruning = {
            # mali prunes its own snapshots.
            keep_sender = [
              {
                type = "regex";
                regex = ".*";
              }
            ];
            keep_receiver = [
              {
                type = "grid";
                grid = "1x24h(keep=all) | 7x1d(keep=1) | 30x1d(keep=1) | 6x30d(keep=1)";
                regex = "^${maliPrefix}";
              }
              (keepNonPrefixed maliPrefix)
            ];
          };
          replication.concurrency = {
            steps = 1;
            size_estimates = 4;
          };
        }
        {
          # Replaces the TrueNAS periodic snapshot tasks with the same names,
          # so existing snapshots age out normally.
          name = "ludwig_snap";
          type = "snap";
          filesystems = ludwig.snapshotFilesystems;
          snapshotting = {
            type = "cron";
            cron = "0 0 * * *";
            prefix = snapshotPrefix;
            # Samba's shadow:format in samba.nix parses this.
            timestamp_format = "2006-01-02_15-04";
            timestamp_location = config.time.timeZone;
          };
          pruning.keep = [
            {
              type = "grid";
              grid = "1x1d(keep=all) | 30x1d";
              regex = "^${snapshotPrefix}";
            }
            (keepNonPrefixed snapshotPrefix)
          ];
        }
      ];
    };
  };

  # healthchecks.io ping, as the TrueNAS cron job did.
  systemd.services.zrepl-mali-received = {
    description = "Report whether recent mali snapshots arrived";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    serviceConfig.Type = "oneshot";
    script = ''
      url=${lib.escapeShellArg ludwig.zreplHealthcheckUrl}
      if ! ${lib.getExe snapshotAge} ${receiveRoot}/tank2 "1 day ago"; then
        url="$url/fail"
      fi
      ${lib.getExe pkgs.curl} -fsS -m 10 --retry 5 -o /dev/null "$url"
    '';
  };
  systemd.timers.zrepl-mali-received = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "*-*-* 13:00:00";
      Persistent = true;
    };
  };
}
