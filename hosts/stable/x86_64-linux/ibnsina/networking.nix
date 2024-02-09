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
      "20-vlmgmt9" = {
        netdevConfig = {
          Kind = "vlan";
          Name = "vlmgmt9";
        };
        vlanConfig = {
          Id = 9;
        };
      };
      "20-vldata11" = {
        netdevConfig = {
          Kind = "vlan";
          Name = "vldata11";
          MTUBytes = "9000";
        };
        vlanConfig = {
          Id = 11;
        };
      };
      "30-brmgmt9" = {
        netdevConfig = {
          Name = "brmgmt9";
          Kind = "bridge";
        };
      };
      "30-brdata11" = {
        netdevConfig = {
          Name = "brdata11";
          Kind = "bridge";
          MTUBytes = "9000";
        };
      };
    };

    networks = {
      "40-eno1" = {
        matchConfig.Name = "eno1";
        vlan = [
          "vlmgmt9"
        ];
      };
      "40-enp6s0" = {
        matchConfig = {Name = "enp6s0";};
        networkConfig = {Description = "10gbe";};
        linkConfig = {MTUBytes = "9000";};
        vlan = [
          "vldata11"
        ];
      };
      "45-vldata11" = {
        matchConfig = {Name = "vldata11";};
        networkConfig = {
          Bridge = "brdata11";
        };
      };
      "45-vlmgmt9" = {
        matchConfig = {Name = "vlmgmt9";};
        networkConfig = {
          Bridge = "brmgmt9";
        };
      };
      "50-brmgmt9" = {
        matchConfig = {Name = "brmgmt9";};
        networkConfig = {
          DHCP = "no";
          Address = "10.9.9.101/23";
          Gateway = "10.9.8.1";
          Description = "mgmt";
        };
      };
      "50-brdata11" = {
        matchConfig = {Name = "brdata11";};
        networkConfig = {
          DHCP = "no";
          Address = "10.9.10.101/23";
          Description = "data";
        };
      };
    };
  };
}
