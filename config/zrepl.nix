{
  config,
  lib,
  ...
}:

let
  inherit (config.repo.secrets) home-ops;
  nodeSettings = config.repo.secrets.global.nodes.${config.networking.hostName};
  cidrToIp = ip: builtins.head (builtins.split "/" ip);
in
{
  options.home-ops.zrepl = {
    enable = lib.mkEnableOption "Enable zrepl";
    sourceAddress = lib.mkOption {
      type = lib.types.str;
      default = cidrToIp nodeSettings.dataCIDR;
      description = "Address on which the replication source listens";
    };
    receiverAddress = lib.mkOption {
      type = lib.types.str;
      default = lib.my.cidrToIp config.repo.secrets.global.nodes.mali.dataCIDR;
      description = "Address of the Mali receiver allowed to pull snapshots";
    };
  };
  config = lib.mkIf config.home-ops.zrepl.enable {
    # zrepl - ZFS snapshotting and replication
    # Every node has a source job and snapshots are pulled by the NAS
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
              listen = "${config.home-ops.zrepl.sourceAddress}:${toString home-ops.ports.zrepl-source}";
              listen_freebind = true;
              clients = {
                "${config.home-ops.zrepl.receiverAddress}" = "mali";
              };
            };
            filesystems = {
              "rpool<" = false;
              "rpool/encrypted/safe/svc<" = true;
              "rpool/encrypted/safe/persist<" = true;
              "rpool/encrypted/safe/vms<" = true;
              "rpool/encrypted/safe/extra/atuin<" = true;
              "tank<" = false;
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
