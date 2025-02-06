{
  lib,
  ...
}:
let
  mergeByKey =
    key: list1: list2:
    let
      # Create a set of items from list2 indexed by the key
      list2Set = builtins.listToAttrs (
        map (item: {
          name = toString item.${key};
          value = item;
        }) list2
      );

      # Process list1: replace items if they exist in list2Set, keep if they don't
      merged = map (
        item:
        if builtins.hasAttr (toString item.${key}) list2Set then list2Set.${toString item.${key}} else item
      ) list1;

      # Add any items from list2 whose keys don't exist in list1
      list1Keys = map (item: toString item.${key}) list1;
      remainingList2 = builtins.filter (item: !(builtins.elem (toString item.${key}) list1Keys)) list2;
    in
    merged ++ remainingList2;
  # Takes a CIDR like "10.9.4.1/22" and returns a list of reverse zones
  toReverseZones =
    cidr:
    let
      # Split CIDR into address and prefix
      parts = builtins.split "/" cidr;
      addr = builtins.elemAt parts 0;
      prefix = builtins.fromJSON (builtins.elemAt parts 2);

      # Split address into octets
      octets = builtins.split "\\." addr;
      firstOctet = builtins.elemAt octets 0;
      secondOctet = builtins.elemAt octets 2;
      thirdOctet = builtins.fromJSON (builtins.elemAt octets 4);

      # Calculate number of /24 networks (2^(24-prefix) if prefix < 24, else 1)
      numNetworks =
        if prefix >= 24 then
          1
        else if prefix == 23 then
          2
        else if prefix == 22 then
          4
        else if prefix == 21 then
          8
        else if prefix == 20 then
          16
        else
          1; # fallback for larger networks

      # Generate list of third octets
      thirdOctets = builtins.genList (x: thirdOctet + x) numNetworks;
    in
    map (third: "${toString third}.${secondOctet}.${firstOctet}.in-addr.arpa.") thirdOctets;

  mkReverseDdns =
    keyName: dnsServers: vlans:
    let
      vlanToReverseDomains =
        vlan:
        map (zone: {
          dns-servers = dnsServers;
          key-name = keyName;
          name = zone;
        }) (toReverseZones vlan.cidr);

      # Combine all VLANs' reverse domains into a single list
      allReverseDomains = builtins.concatLists (
        builtins.attrValues (builtins.mapAttrs (name: vlan: vlanToReverseDomains vlan) vlans)
      );
    in
    allReverseDomains;

  # Helper function to generate a single VLAN subnet configuration
  mkVlanSubnet =
    {
      name,
      vlanConfig,
      commonDhcpOptions,
      leaseOption,
    }:
    let
      ddnsEnabled = vlanConfig.dhcp ? domainName;
    in
    lib.mkIf vlanConfig.dhcp.enable (
      {
        id = if name == "untagged" then 1 else vlanConfig.id;
        # TODO: this is a hack, fix this eventually, why didn't I leave unttaged in the vlan attrset again?
        interface = if name == "untagged" then "lan0" else "me-${name}";
        subnet = vlanConfig.cidr;
        pools = [
          {
            pool = "${vlanConfig.dhcp.start} - ${vlanConfig.dhcp.stop}";
          }
        ];
        hostname-char-set = "[^A-Za-z0-9.-]";
        hostname-char-replacement = "-";
        ddns-send-updates = ddnsEnabled;
        ddns-replace-client-name = "when-present";
        ddns-generated-prefix = "";
        ddns-qualifying-suffix = lib.mkIf ddnsEnabled "${vlanConfig.dhcp.domainName}.";
        option-data =
          [
            {
              name = "routers";
              data = lib.my.cidrToIp vlanConfig.cidr;
            }
          ]
          ++ (
            if ddnsEnabled then
              [
                {
                  name = "domain-name";
                  data = vlanConfig.dhcp.domainName;
                }
                {
                  name = "domain-search";
                  data = vlanConfig.dhcp.domainName;
                }
              ]
            else
              [ ]
          )
          ++ (
            if vlanConfig.dhcp ? optionData then
              (mergeByKey "name" commonDhcpOptions vlanConfig.dhcp.optionData)
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
      }
    ) (lib.mapAttrsToList lib.nameValuePair enabledVlans);
  joinList = lib.concatStringsSep ", ";
in
{
  inherit
    mkVlanSubnet
    mkKeaSubnets
    joinList
    toReverseZones
    mkReverseDdns
    ;
}
