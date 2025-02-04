{
  lib,
  ...
}:
let
  # Helper function to generate a single VLAN subnet configuration
  mkVlanSubnet =
    {
      name,
      vlanConfig,
      id,
      commonDhcpOptions,
      leaseOption,
    }:
    lib.mkIf vlanConfig.dhcp.enable (
      {
        id = id;
        interface = vlanConfig.iface;
        subnet = vlanConfig.cidr;
        pools = [
          {
            pool = "${vlanConfig.dhcp.start} - ${vlanConfig.dhcp.stop}";
          }
        ];
        option-data =
          [
            {
              name = "routers";
              data = lib.my.cidrToIp vlanConfig.cidr;
            }
          ]
          ++ (
            if vlanConfig.dhcp ? option-data then
              lib.unique (vlanConfig.dhcp.option-data ++ commonDhcpOptions)
            else
              commonDhcpOptions
          );
        reservations = lib.optionals (vlanConfig.dhcp ? reservations) vlanConfig.dhcp.reservations;
      }
      // leaseOption
      // (lib.optionalAttrs (vlanConfig.dhcp ? extraConfig) vlanConfig.dhcp.extraConfig)
    );

  # Main function to generate all VLAN subnet configurations
  mkKeaSubnets =
    {
      vlans,
      commonDhcpOptions,
      leaseOption,
    }:
    let
      enabledVlans = lib.filterAttrs (name: vlan: vlan.dhcp.enable) vlans;
    in
    lib.imap0 (
      index: nameValuePair:
      mkVlanSubnet {
        vlanConfig = nameValuePair.value;
        name = nameValuePair.name;
        inherit
          commonDhcpOptions
          leaseOption
          ;
        id = index + 1; # Start IDs at 1
      }
    ) (lib.mapAttrsToList lib.nameValuePair enabledVlans);
in
{
  inherit mkVlanSubnet mkKeaSubnets;
}
