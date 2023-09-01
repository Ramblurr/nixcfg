{
  config,
  pkgs,
  inputs,
  unstable,
  lib,
  ...
}: {
  networking.firewall.allowedTCPPorts = [9811 3478];
  services.zrepl = {
    enable = true;
    # my remote zrepl host uses the latest zrepl
    package = unstable.zrepl;
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
          name = "mali_source";
          type = "source";
          serve = {
            type = "tls";
            listen = "10.9.10.10:3478";
            ca = "/mnt/tank2/iocage/jails/zrepl/root/usr/local/etc/zrepl/ludwig.crt";
            cert = "/mnt/tank2/iocage/jails/zrepl/root/usr/local/etc/zrepl/mali.crt";
            key = "/mnt/tank2/iocage/jails/zrepl/root/usr/local/etc/zrepl/mali.key";
            client_cns = ["ludwig"];
          };
          filesystems = {
            "rpool<" = false;
            "tank/backup<" = false;
            "tank2<" = true;
            "tank2/media<" = false;
            "tank2/media/music/mine" = true;
            "tank2/replication<" = false;
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
