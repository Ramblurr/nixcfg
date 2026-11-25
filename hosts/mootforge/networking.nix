{ config, lib, ... }:
let
  inherit (config.networking) hostName;
  inherit (config.repo.secrets) local;
  hostConfig = config.site.hosts.${hostName};
  homeDomain = config.repo.secrets.global.domain.home;
in
{
  inherit (config.repo.secrets.site) site;
  networking.nameservers = config.repo.secrets.global.nameservers;
  # No prim address here, so mali would answer its prim address over mgmt, bypassing the router.
  networking.hosts.${builtins.head config.site.net.data.hosts4.mali} = [
    "nix-cache.int.${homeDomain}"
    "attic.int.${homeDomain}"
  ];
  time.timeZone = "Europe/Berlin";

  modules.vpn.tailscale.enable = true;
  services.tailscale = {
    authKeyFile = "/persist/etc/tailscale/authkey";
    # The tailnet DNS view sends the forge to an address this host cannot reach.
    extraSetFlags = [
      "--ssh"
      "--accept-dns=false"
    ];
  };

  systemd.network = {
    links = {
      "10-lan0" = {
        matchConfig.MACAddress = config.repo.secrets.site.site.hosts.${hostName}.interfaces.lan0.hwaddr;
        linkConfig.Name = "lan0";
      };
      "10-lan1" = {
        matchConfig.MACAddress = local.lan1.hwaddr;
        linkConfig.Name = "lan1";
      };
    };
    networks."10-lan1" = {
      matchConfig.Name = "lan1";
      networkConfig = {
        DHCPServer = false;
        VLAN = map (net: "vlan-${net}") (
          lib.attrNames (lib.filterAttrs (_: iface: iface.parent == "lan1") hostConfig.interfaces)
        );
        LinkLocalAddressing = false;
        LLDP = true;
        EmitLLDP = true;
      };
      linkConfig = {
        MTUBytes = 9000;
        RequiredForOnline = "carrier";
      };
    };
  };
}
