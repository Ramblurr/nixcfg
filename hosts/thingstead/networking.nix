{ config, ... }:
let
  inherit (config.repo.secrets) local;
  inherit (config.site.net) data;
in
{
  networking.useDHCP = false;
  services.timesyncd.enable = true;
  services.resolved.enable = true;
  time.timeZone = "Europe/Berlin";
  systemd.network = {
    enable = true;
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
        matchConfig.MACAddress = local.eno1.hwaddr;
        linkConfig.Name = "eno1";
      };
      "10-enp6s0" = {
        matchConfig.MACAddress = local.enp6s0.hwaddr;
        linkConfig.Name = "enp6s0";
      };
    };
    networks = {
      "10-eno1" = {
        matchConfig.Name = "eno1";
        networkConfig.DHCP = "ipv4";
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
