{ config, lib, ... }:
let
  dhcpLib = import ./helpers.nix { inherit lib; };
  leaseOption = {
    valid-lifetime = 86400;
    renew-timer = 43200; # 50% of valid lifetime
    rebind-timer = 75600; # 87.5% of valid lifetime
  };
  commonDhcpOptions = [
    {
      name = "domain-name-servers";
      data = lib.my.cidrToIp config.repo.secrets.local.vlan.local.cidr;
    }
    {
      name = "time-servers";
      data = lib.my.cidrToIp config.repo.secrets.local.vlan.local.cidr;
    }
    {
      name = "domain-name";
      data = "home.arpa";
    }
    {
      name = "domain-search";
      data = "home.arpa";
    }
  ];

in
{
  services.kea.dhcp4 = {
    enable = true;
    settings = {
      interfaces-config = {
        interfaces = lib.mapAttrsToList (name: vlan: vlan.iface) config.repo.secrets.local.vlan;
      };
      subnet4 = dhcpLib.mkKeaSubnets {
        vlans = config.repo.secrets.local.vlan;
        inherit commonDhcpOptions leaseOption;
      };
    };
  };
}
