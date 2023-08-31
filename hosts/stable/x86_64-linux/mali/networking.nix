{
  config,
  lib,
  pkgs,
  ...
}: {
  networking.usePredictableInterfaceNames = true;

  systemd.network = {
    netdevs = {
      "10-lagg0" = {
        netdevConfig = {
          Name = "lagg0";
          Kind = "bond";
          MACAddress = "00:25:90:bb:78:3e";
          MTUBytes = "1500";
          Description = "Lagg0 bonded interface";
        };
        bondConfig = {
          Mode = "802.3ad";
        };
      };
      "20-vlan4" = {
        netdevConfig = {
          Name = "vlan4";
          Kind = "vlan";
          MTUBytes = "1500";
        };
        vlanConfig = {
          Id = 4;
        };
      };
      "30-bridge4" = {
        netdevConfig = {
          Name = "bridge4";
          Kind = "bridge";
          MTUBytes = "1500";
        };
      };
      "40-vlan9" = {
        netdevConfig = {
          Name = "vlan9";
          Kind = "vlan";
          MTUBytes = "1500";
        };
        vlanConfig = {
          Id = 9;
        };
      };
      "50-bridge9" = {
        netdevConfig = {
          Name = "bridge9";
          Kind = "bridge";
          MTUBytes = "1500";
        };
      };
      "70-vlan11" = {
        netdevConfig = {
          Name = "vlan11";
          Kind = "vlan";
          MTUBytes = "9000";
        };
        vlanConfig = {
          Id = 11;
        };
      };
      "80-bridge11" = {
        netdevConfig = {
          Name = "bridge11";
          Kind = "bridge";
          MTUBytes = "9000";
        };
      };
    };

    networks = {
      "10-lagg0" = {
        matchConfig = {name = "lagg0";};
      };
      "11-eno1" = {
        matchConfig = {name = "eno1";};
        networkConfig = {Bond = "lagg0";};
      };
      "12-eno1" = {
        matchConfig = {name = "eno2";};
        networkConfig = {Bond = "lagg0";};
      };
      "20-vlan4" = {
        matchConfig = {name = "lagg0";};
        networkConfig = {VLAN = "vlan4";};
      };
      "30-bridge4" = {
        matchConfig = {name = "vlan4";};
        networkConfig = {
          Bridge = "bridge4";
          Address = "10.9.4.10/22";
          Description = "primary";
          Gateway = "10.9.4.1";
        };
        routes = [
          {
            routeConfig = {
              Destination = "192.168.8.0/22";
              Gateway = "10.9.4.27";
            };
          }
        ];
      };
      "40-vlan9" = {
        matchConfig = {name = "lagg0";};
        networkConfig = {VLAN = "vlan9";};
      };
      "50-bridge9" = {
        matchConfig = {name = "vlan9";};
        networkConfig = {
          Bridge = "bridge9";
          Address = "10.9.8.3/23";
          Description = "mgmt";
        };
      };
      "60-enp1s0f0" = {
        matchConfig = {name = "enp1s0f0";};
        networkConfig = {Description = "10gbe";};
        linkConfig = {MTUBytes = "9000";};
      };
      "70-vlan11" = {
        matchConfig = {name = "enp1s0f0";};
        networkConfig = {VLAN = "vlan11";};
      };
      "80-bridge11" = {
        matchConfig = {name = "vlan11";};
        networkConfig = {
          Bridge = "bridge11";
          Address = "10.9.10.10/24";
          Description = "data";
        };
      };
    };
  };
}
