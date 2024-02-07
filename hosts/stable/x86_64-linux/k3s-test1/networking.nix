{
  config,
  lib,
  pkgs,
  ...
}: {
  networking.usePredictableInterfaceNames = true;
  networking.firewall.allowPing = true;
  networking.nameservers = ["192.168.1.3" "10.9.4.4"];

  # Useful if you need to troubleshoot systemd-networkd
  # systemd.services.systemd-networkd.serviceConfig.Environment = ["SYSTEMD_LOG_LEVEL=debug"];

  systemd.network = {
    netdevs = {
    };

    networks = {
      "10-enp0s9" = {
        matchConfig = {Name = "enp0s9";};
        networkConfig = {
          DHCP = "no";
          Address = "10.0.2.15/22";
          Gateway = "10.0.2.2";
        };
      };
    };
  };
}
