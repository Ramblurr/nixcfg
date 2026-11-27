{ config, ... }:
let
  inherit (config.repo.secrets) local;
  inherit (config.site.net) data mgmt;
in
{
  networking.useDHCP = false;
  networking.nameservers = config.repo.secrets.global.nameservers;
  services.timesyncd.enable = true;
  services.resolved.enable = true;
  time.timeZone = "Europe/Berlin";
  systemd.network = {
    enable = true;
    netdevs."20-mgmt" = {
      netdevConfig = {
        Kind = "vlan";
        Name = "mgmt";
      };
      vlanConfig.Id = mgmt.vlan;
    };
    netdevs."20-data" = {
      netdevConfig = {
        Kind = "vlan";
        Name = "data";
        MTUBytes = data.mtu;
      };
      vlanConfig.Id = data.vlan;
    };
    links = {
      "10-eno1" = {
        matchConfig.PermanentMACAddress = local.eno1.hwaddr;
        linkConfig.Name = "eno1";
      };
      "10-enp6s0" = {
        matchConfig.PermanentMACAddress = local.enp6s0.hwaddr;
        linkConfig.Name = "enp6s0";
      };
    };
    networks = {
      "10-eno1" = {
        matchConfig.Name = "eno1";
        networkConfig = {
          VLAN = [ "mgmt" ];
          LinkLocalAddressing = false;
        };
        linkConfig.RequiredForOnline = "carrier";
      };
      "10-enp6s0" = {
        matchConfig.Name = "enp6s0";
        networkConfig = {
          VLAN = [ "data" ];
          LinkLocalAddressing = false;
        };
        linkConfig = {
          MTUBytes = data.mtu;
          RequiredForOnline = false;
        };
      };
      "30-mgmt" = {
        matchConfig.Name = "mgmt";
        address = map (a: "${a}/${toString mgmt.subnet4Len}") mgmt.hosts4.thingstead;
        gateway = mgmt.hosts4.${mgmt.dhcp.router};
        networkConfig.LinkLocalAddressing = false;
      };
      "30-data" = {
        matchConfig.Name = "data";
        address = map (a: "${a}/${toString data.subnet4Len}") data.hosts4.thingstead;
        networkConfig.LinkLocalAddressing = false;
        linkConfig.RequiredForOnline = false;
      };
      "10-wlp0s20f3" = {
        matchConfig.Name = "wlp0s20f3";
        networkConfig.ConfigureWithoutCarrier = true;
        linkConfig = {
          ActivationPolicy = "always-down";
          Unmanaged = true;
        };
      };
      "20-tailscale-ignore" = {
        matchConfig.Name = "tailscale*";
        linkConfig = {
          RequiredForOnline = false;
          Unmanaged = true;
        };
      };
    };
  };
}
