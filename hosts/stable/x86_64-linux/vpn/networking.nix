{lib, ...}: let
  vpn = builtins.fromJSON (builtins.readFile ../../../../secrets/vpn.secrets);
in {
  networking = {
    nameservers = vpn.nameservers;
    defaultGateway = vpn.ipv4-gateway;
    defaultGateway6 = {
      address = "fe80::1";
      interface = "eth0";
    };
    dhcpcd.enable = false;
    usePredictableInterfaceNames = lib.mkForce false;
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          {
            address = vpn.ipv4;
            prefixLength = 32;
          }
        ];
        ipv6.addresses = [
          {
            address = vpn.ipv6-1;
            prefixLength = 64;
          }
          {
            address = vpn.ipv6-2;
            prefixLength = 64;
          }
        ];
        ipv4.routes = [
          {
            address = vpn.ipv4-route;
            prefixLength = 32;
          }
        ];
        ipv6.routes = [
          {
            address = "fe80::1";
            prefixLength = 128;
          }
        ];
      };
    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="${vpn.eth0}", NAME="eth0"

  '';
}
