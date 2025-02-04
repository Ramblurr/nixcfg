{
  config,
  pkgs,
  lib,
  ...
}:
{

  networking = {
    hostId = pkgs.lib.concatStringsSep "" (
      pkgs.lib.take 8 (
        pkgs.lib.stringToCharacters (builtins.hashString "sha256" config.networking.hostName)
      )
    );
    domain = config.repo.secrets.global.domain.home;
    useDHCP = false;
  };
  systemd.network = {
    enable = true;
    wait-online = {
      anyInterface = false;
      ignoredInterfaces = [
        "ve-+"
        "wan0"
        "lan1"
        "ctr0"
        "wg0"
      ];
    };

    links = {
      # rename all interface names to be easier to identify
      "10-wan0" = {
        # 00:1f.6 Ethernet controller: Intel Corporation Ethernet Connection (7) I219-V (rev 10)
        #matchConfig.Path = "pci-0000:00:1f.6";
        matchConfig.MACAddress = config.repo.secrets.local.wan0.mac;
        linkConfig.Name = "wan0";
      };
      "10-lan0" = {
        # 01:00.0 Ethernet controller: Mellanox Technologies MT27520 Family [ConnectX-3 Pro]
        # left SFP+ port
        matchConfig.MACAddress = config.repo.secrets.local.lan0.mac;
        linkConfig.Name = "lan0";
      };
      "10-lan1" = {
        # 01:00.0 Ethernet controller: Mellanox Technologies MT27520 Family [ConnectX-3 Pro]
        # right SFP+ port
        matchConfig.MACAddress = config.repo.secrets.local.lan1.mac;
        linkConfig.Name = "lan1";
      };
      #"10-testnet" = {
      #  matchConfig.MACAddress = "8c:1d:2d:bf:df:53";
      #  linkConfig.Name = "testnet";
      #};
    };
    netdevs = {
      # VLANs
      "20-vlguest3" = {
        netdevConfig = {
          Name = "vlguest3";
          Description = "GUEST";
          Kind = "vlan";
          MTUBytes = "1500";
        };
        vlanConfig.Id = 3;
      };
      "20-vlprim4" = {
        netdevConfig = {
          Name = "vlprim4";
          Description = "PRIMARY";
          Kind = "vlan";
          MTUBytes = "1500";
        };
        vlanConfig.Id = 4;
      };
      "20-vlmgmt9" = {
        netdevConfig = {
          Name = "vlmgmt9";
          Description = "MGMT";
          Kind = "vlan";
          MTUBytes = "1500";
        };
        vlanConfig.Id = 9;
      };
      "20-vldata11" = {
        netdevConfig = {
          Name = "vldata11";
          Description = "DATA";
          Kind = "vlan";
          MTUBytes = "9000";
        };
        vlanConfig.Id = 11;
      };
      "20-vliot50" = {
        netdevConfig = {
          Name = "vliot50";
          Description = "IOT";
          Kind = "vlan";
          MTUBytes = "1500";
        };
        vlanConfig.Id = 50;
      };
      "20-vlnot60" = {
        netdevConfig = {
          Name = "vlnot60";
          Description = "NOT";
          Kind = "vlan";
          MTUBytes = "1500";
        };
        vlanConfig.Id = 60;
      };
    };
    networks = {
      "30-wlp3s0" = {
        matchConfig.Name = "wlp3s0";
        linkConfig = {
          Unmanaged = "yes";
          ActivationPolicy = "down";
        };
      };

      # Disabled interfaces
      "30-lan1" = {
        matchConfig.Name = "lan1";
        networkConfig.ConfigureWithoutCarrier = true;
        linkConfig.ActivationPolicy = "always-down";
        networkConfig.DHCPServer = false;
      };
      # WAN0
      "30-wan0" = {
        matchConfig.Name = "wan0";
        address = [ config.repo.secrets.local.wan0.cidr ];
        networkConfig = {
          Gateway = config.repo.secrets.local.wan0.gateway;
          DHCPServer = false;
        };
        linkConfig = {
          MTUBytes = "1500";
          RequiredForOnline = "routable";
        };
      };
      # LAN0
      "30-lan0" = {
        matchConfig.Name = "lan0";
        address = [
          config.repo.secrets.local.vlan.local.cidr
          "192.168.1.3/24"
        ];
        networkConfig.DHCPServer = false;
        linkConfig = {
          MTUBytes = "9000";
          RequiredForOnline = "carrier";
        };
        vlan = [
          "vlguest3"
          "vlprim4"
          "vlmgmt9"
          "vldata11"
          "vliot50"
          "vlnot60"
        ];
      };
      # GUEST VLAN
      "30-vlguest3" = {
        matchConfig.Name = "vlguest3";
        address = [ config.repo.secrets.local.vlan.guest.cidr ];
        networkConfig.DHCPServer = false;
        linkConfig.RequiredForOnline = "routable";
      };
      # PRIMARY VLAN
      "30-vlprim4" = {
        matchConfig.Name = "vlprim4";
        address = [
          config.repo.secrets.local.vlan.prim.cidr
          "10.9.4.4/22"
        ];
        networkConfig.DHCPServer = false;
        linkConfig.RequiredForOnline = "routable";
      };
      # MGMT VLAN
      "30-vlmgmt9" = {
        matchConfig.Name = "vlmgmt9";
        address = [ config.repo.secrets.local.vlan.mgmt.cidr ];
        networkConfig.DHCPServer = false;
        linkConfig.RequiredForOnline = "routable";
      };
      # DATA VLAN
      "30-vldata11" = {
        matchConfig.Name = "vldata11";
        address = [ config.repo.secrets.local.vlan.data.cidr ];
        networkConfig.DHCPServer = false;
        linkConfig.RequiredForOnline = "routable";
      };
      # IOT VLAN
      "30-vliot50" = {
        matchConfig.Name = "vliot50";
        address = [ config.repo.secrets.local.vlan.iot.cidr ];
        networkConfig.DHCPServer = false;
        linkConfig.RequiredForOnline = "routable";
      };
      # NOT VLAN
      "30-vlnot60" = {
        matchConfig.Name = "vlnot60";
        address = [ config.repo.secrets.local.vlan.not.cidr ];
        networkConfig.DHCPServer = false;
        linkConfig.RequiredForOnline = "routable";
      };
      #"30-testnet" = {
      #  matchConfig.Name = "testnet";
      #  linkConfig = {
      #    MTUBytes = "1500";
      #    RequiredForOnline = "carrier";
      #  };
      #  networkConfig = {
      #    DHCP = "yes";
      #    DNSSEC = "no";
      #  };
      #};
    };
  };
}
