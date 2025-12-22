{
  lib,
  ...
}:
let
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
        builtins.attrValues (builtins.mapAttrs (_name: vlanToReverseDomains) vlans)
      );
    in
    allReverseDomains;

  joinList = lib.concatStringsSep ", ";
in
{
  inherit
    joinList
    toReverseZones
    mkReverseDdns
    ;
}
