{
  config,
  pkgs,
  inputs,
  unstable,
  lib,
  ...
}:
#
# ZFS Replication with zrepl (https://github.com/zrepl/zrepl)
#
# My NAS runs pull jobs for local nodes (they have corresponding source jobs)
# These are unencrypted and run over my local 10gbe data vlan.
#
# My NAS also has local source job which is pulled by another offsite NAS (not managed here)
#
# When creating the zfs replication root (tank2/replication) create it with:
#     sudo zfs create tank2/replication -o compression=zstd -o mountpoint=none -o canmount=off
let
  cidrToIp = ip: builtins.head (builtins.split "/" ip);
in
{
  networking.firewall.allowedTCPPorts = [
    9811
    3478
  ];

  sops.secrets."zrepl/ludwigCert" = { };
  sops.secrets."zrepl/maliCert" = { };
  sops.secrets."zrepl/maliKey" = { };
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
            listen = ":${toString config.repo.secrets.home-ops.ports.zrepl-metrics}";
            listen_freebind = true;
          }
        ];
      };

      jobs =
        let
          nodesWithZReplSources = lib.filterAttrs (
            name: node: node ? zreplSource && node.zreplSource == true
          ) config.repo.secrets.global.nodes;
          mkNodePullJob = name: node: {
            name = "${name}";
            type = "pull";
            interval = "1h";
            root_fs = "tank2/replication/${name}";
            connect = {
              type = "tcp";
              address = "${cidrToIp node.dataCIDR}:${toString config.repo.secrets.home-ops.ports.zrepl-source}";
              dial_timeout = "10s";
            };
            recv = {
              properties = {
                override = {
                  #mountpoint = "none";
                  #canmount = "off";
                  readonly = "on";
                };
              };

              "placeholder" = {
                encryption = "inherit";
              };
            };
            pruning = {
              keep_sender = [
                { type = "not_replicated"; }
                {
                  type = "last_n";
                  count = 7;
                }
                {
                  type = "grid";
                  # of the last 24 hours keep all snapshots
                  # of the last 7 days keep 1 snapshot each day
                  # of the last 30 days keep 1 snapshot each day
                  # of the last 6 months keep 1 snapshot each month
                  # of the last 1 year keep 1 snapshot each year
                  # discard the rest
                  # details see: https://zrepl.github.io/configuration/prune.html#policy-grid
                  grid = "1x24h(keep=all) | 7x1d(keep=1) | 30x1d(keep=1) | 6x30d(keep=1) | 1x365d(keep=1)";
                  regex = "^zrepl_.*";
                }
                # !! keep snapshots not created by zrepl
                {
                  type = "regex";
                  negate = true;
                  regex = "^zrepl_.*";
                }
              ];
              keep_receiver = [
                {
                  type = "grid";
                  # of the last 24 hours keep all snapshots
                  # of the last 7 days keep 1 snapshot each day
                  # of the last 30 days keep 1 snapshot each day
                  # of the last 6 months keep 1 snapshot each month
                  # of the last 1 year keep 1 snapshot each year
                  # discard the rest
                  # details see: https://zrepl.github.io/configuration/prune.html#policy-grid
                  grid = "1x24h(keep=all) | 7x1d(keep=1) | 30x1d(keep=1) | 6x30d(keep=1) | 1x365d(keep=1)";
                  regex = "^zrepl_.*";
                }
                # keep snapshots not created by zrepl
                {
                  type = "regex";
                  negate = true;
                  regex = "^zrepl_.*";
                }
              ];
            };
            replication = {
              concurrency = {
                steps = 1;
                size_estimates = 4;
              };
            };
          };
          nodeJobs = lib.mapAttrsToList mkNodePullJob nodesWithZReplSources;
        in
        nodeJobs
        ++ [
          {
            name = "mali_source";
            type = "source";
            serve = {
              type = "tls";
              listen = "10.9.10.10:3478";
              ca = config.sops.secrets."zrepl/ludwigCert".path;
              cert = config.sops.secrets."zrepl/maliCert".path;
              key = config.sops.secrets."zrepl/maliKey".path;
              client_cns = [ "ludwig" ];
            };
            filesystems = {
              "rpool<" = false;
              "tank/backup<" = false;
              "tank2<" = true;
              "tank2/media<" = false;
              "tank2/media/music/mine" = true;
              "tank2/replication<" = false;
              "tank2/proxmox<" = false;
              "tank2/backups/gamsjaegers<" = false;
            };
            # "send.compressed" = true;
            snapshotting = {
              type = "periodic";
              prefix = "zrepl_";
              interval = "6h";
            };
          }
        ];
    };
  };
}
