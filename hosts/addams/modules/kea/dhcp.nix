{ config, lib, ... }:
let
  dhcpLib = import ./helpers.nix { inherit lib; };
  leaseOption =
    {
    };
  commonDhcpOptions = [
    {
      name = "domain-name-servers";
      data = dhcpLib.joinList config.repo.secrets.local.dns.internal;
    }
    {
      name = "time-servers";
      data = lib.my.cidrToIp config.repo.secrets.local.untagged.cidr;
    }
  ];

in
{
  environment.persistence."/persist".directories = [ "/var/lib/private/kea" ];
  services.kea.dhcp4 = {
    enable = true;
    settings = {
      lease-database = {
        name = "/var/lib/kea/kea-leases4.csv";
        persist = true;
        type = "memfile";
      };
      valid-lifetime = 86400;
      renew-timer = 3600;
      interfaces-config = {
        dhcp-socket-type = "raw";
        interfaces = [
          # TODO: this is a hack, fix this eventually, why didn't I leave unttaged in the vlan attrset again?
          "lan0"
        ] ++ lib.mapAttrsToList (name: _: "me-${name}") config.repo.secrets.local.vlan;
      };
      match-client-id = false;
      sanity-checks = {
        lease-checks = "fix-del";
      };
      subnet4 = dhcpLib.mkKeaSubnets {
        vlans = config.repo.secrets.local.vlan // {
          "untagged" = config.repo.secrets.local.untagged;
        };
        inherit commonDhcpOptions leaseOption;
      };
    };
  };
}
