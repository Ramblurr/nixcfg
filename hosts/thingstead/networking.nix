{ config, ... }:
let
  inherit (config.repo.secrets) local;
in
{
  networking.useDHCP = false;
  services.timesyncd.enable = true;
  services.resolved.enable = true;
  time.timeZone = "Europe/Berlin";
  systemd.network = {
    enable = true;
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
      # addams reserves the moot address for this NIC by MAC.
      "10-eno1" = {
        matchConfig.Name = "eno1";
        networkConfig.DHCP = "ipv4";
      };
      "10-enp6s0" = {
        matchConfig.Name = "enp6s0";
        linkConfig = {
          ActivationPolicy = "always-down";
          Unmanaged = true;
        };
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
