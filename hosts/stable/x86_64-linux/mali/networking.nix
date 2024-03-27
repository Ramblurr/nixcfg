{ config, lib, pkgs, ... }: {
  networking.usePredictableInterfaceNames = true;
  networking.firewall.allowPing = true;
  networking.nameservers = [ "192.168.1.3" "10.9.4.4" ];

  # Useful if you need to troubleshoot systemd-networkd
  # systemd.services.systemd-networkd.serviceConfig.Environment = ["SYSTEMD_LOG_LEVEL=debug"];

  systemd.network = {
    netdevs = {
      "10-bond0" = {
        netdevConfig = {
          Name = "bond0";
          Kind = "bond";
          #MTUBytes = "1500";
          Description = "Bond0 bonded interface";
        };
        bondConfig = {
          Mode = "802.3ad";
          MIIMonitorSec = "1s";
          LACPTransmitRate = "fast";
          UpDelaySec = "2s";
          DownDelaySec = "8s";
          TransmitHashPolicy = "layer3+4";
        };
      };
      "20-vlprim4" = {
        netdevConfig = {
          Name = "vlprim4";
          Kind = "vlan";
          MTUBytes = "1500";
        };
        vlanConfig = { Id = 4; };
      };
      "30-brprim4" = {
        netdevConfig = {
          Name = "brprim4";
          Kind = "bridge";
          MTUBytes = "1500";
        };
      };
      "40-vlmgmt9" = {
        netdevConfig = {
          Name = "vlmgmt9";
          Kind = "vlan";
          MTUBytes = "1500";
        };
        vlanConfig = { Id = 9; };
      };
      "50-brmgmt9" = {
        netdevConfig = {
          Name = "brmgmt9";
          Kind = "bridge";
          MTUBytes = "1500";
        };
      };
      "70-vldata11" = {
        netdevConfig = {
          Name = "vldata11";
          Kind = "vlan";
          MTUBytes = "9000";
        };
        vlanConfig = { Id = 11; };
      };
      "80-brdata11" = {
        netdevConfig = {
          Name = "brdata11";
          Kind = "bridge";
          MTUBytes = "9000";
        };
      };
    };

    networks = {
      "10-bond0" = {
        matchConfig = { Name = "bond0"; };
        vlan = [ "vlmgmt9" "vlprim4" ];
        networkConfig = {
          BindCarrier = "eno1 eno2";
          DHCP = "no";
          LinkLocalAddressing = "no";
        };
      };
      "11-eno1" = {
        matchConfig = { Name = "eno1"; };
        networkConfig = { Bond = "bond0"; };
      };
      "12-eno1" = {
        matchConfig = { Name = "eno2"; };
        networkConfig = { Bond = "bond0"; };
      };
      "20-vlprim4" = {
        matchConfig = { Name = "vlprim4"; };
        networkConfig = { Bridge = "brprim4"; };
      };
      "30-brprim4" = {
        matchConfig = { Name = "brprim4"; };
        networkConfig = {
          DHCP = "no";
          Address = "10.9.4.10/22";
          Description = "primary";
          DNSSEC = "no";
        };
        routes = [{
          routeConfig = {
            Destination = "192.168.8.0/22";
            Gateway = "10.9.4.27";
          };
        }];
      };
      "40-vlmgmt9" = {
        matchConfig = { Name = "vlmgmt9"; };
        networkConfig = { Bridge = "brmgmt9"; };
      };
      "50-brmgmt9" = {
        matchConfig = { Name = "brmgmt9"; };
        networkConfig = {
          DHCP = "no";
          Address = "10.9.8.3/23";
          Description = "mgmt";
        };
      };
      "60-enp2s0f1" = {
        matchConfig = { Name = "enp2s0f1"; };
        networkConfig = { Description = "10gbe"; };
        linkConfig = { MTUBytes = "9000"; };
        vlan = [ "vldata11" ];
      };
      "70-vldata11" = {
        matchConfig = { Name = "vldata11"; };
        networkConfig = { Bridge = "brdata11"; };
      };
      "80-brdata11" = {
        matchConfig = { Name = "brdata11"; };
        networkConfig = {
          DHCP = "no";
          Address = "10.9.10.10/24";
          Description = "data";
          Gateway = "10.9.10.1";
          DNSSEC = "no";
        };
      };
    };
  };
}
