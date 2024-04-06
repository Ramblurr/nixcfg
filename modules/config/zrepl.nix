{
  config,
  lib,
  pkgs,
  ...
}:

let
  home-ops = config.repo.secrets.home-ops;
  cidrToIp = ip: builtins.head (builtins.split "/" ip);
in
{

  options.home-ops.zrepl = {
    enable = lib.mkEnableOption "Enable zrepl";
  };
  config = lib.mkIf config.home-ops.zrepl.enable {
    # zrepl - ZFS snapshotting and replication
    # Every node has a source job and snapshots are pulled by my NAS
    networking.firewall.allowedTCPPorts = [
      home-ops.ports.zrepl-metrics
      home-ops.ports.zrepl-source
    ];
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
              listen = ":${toString home-ops.ports.zrepl-metrics}";
              listen_freebind = true;
            }
          ];
        };

        jobs = [
          {
            name = "${config.networking.hostName}_source";
            type = "source";
            serve = {
              type = "tcp";
              listen = "${cidrToIp config.repo.secrets.local.dataCIDR}:${toString home-ops.ports.zrepl-source}";
              listen_freebind = true;
              clients = {
                "${config.repo.secrets.global.nodes.mali.data}" = "mali";
              };
            };
            filesystems = {
              "rpool<" = false;
              "rpool/encrypted/safe/svc<" = true;
              "rpool/encrypted/safe/persist<" = false; # TODO: enable after de-k8sifying
              "rpool/encrypted/safe/vms<" = true;
              "rpool/encrypted/safe/extra/atuin<" = true;
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
  };
}
