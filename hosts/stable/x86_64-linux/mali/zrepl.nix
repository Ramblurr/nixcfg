{ config, pkgs, inputs, unstable, lib, ... }: {
  networking.firewall.allowedTCPPorts = [ 9811 3478 ];

  sops.secrets."zrepl/ludwigCert" = { };
  sops.secrets."zrepl/maliCert" = { };
  sops.secrets."zrepl/maliKey" = { };
  services.zrepl = {
    enable = true;
    settings = {
      global = {
        logging = [{
          type = "syslog";
          level = "info";
          format = "human";
        }];
        monitoring = [{
          type = "prometheus";
          listen = ":9811";
          listen_freebind = true;
        }];
      };

      jobs = [{
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
      }];
    };
  };
}
