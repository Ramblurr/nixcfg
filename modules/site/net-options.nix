{
  lib,
  config,
  ...
}:
let

  inherit (lib)
    elemAt
    mkOption
    mkEnableOption
    types
    ;

in
{ name, ... }:
{

  options = {
    vlan = mkOption {
      description = "VLAN tag number";
      type = types.nullOr types.int;
    };
    subnet4 = mkOption {
      description = "v.w.x.y/z";
      type = types.nullOr types.str;
      default = null;
    };
    subnet4Net = mkOption {
      type = types.nullOr types.str;
      default =
        let
          inherit (config.site.net.${name}) subnet4;
          s = lib.splitString "/" subnet4;
        in
        if subnet4 != null && builtins.length s == 2 then builtins.head s else null;
      readOnly = true;
    };
    subnet4Len = mkOption {
      type = types.nullOr types.int;
      default =
        let
          inherit (config.site.net.${name}) subnet4;
          s = lib.splitString "/" subnet4;
        in
        if subnet4 != null && builtins.length s == 2 then lib.toInt (elemAt s 1) else null;
      readOnly = true;
    };
    subnets6 = mkOption {
      description = "The /64 IPv6 subnets";
      type = types.attrsOf types.str;
      default = { };
    };
    hosts4 = mkOption {
      description = "Attribute set of hostnames to IPv4 addresses";
      type = types.attrsOf (types.listOf types.str);
      default = { };
    };
    hosts6 = mkOption {
      description = "Attribute set of contexts to attribute sets of hostnames to IPv4 addresses";
      type = types.attrsOf (types.attrsOf (types.listOf types.str));
      default = { };
    };
    #ospf = {
    #  secret = mkOption {
    #    type = types.nullOr types.str;
    #    default = null;
    #  };
    #};
    dhcp = {
      enable = mkEnableOption "Enable DHCP on this network";
      id = mkOption {
        description = "The subnet identifier (subnet ID) is a unique number associated with a particular subnet. The server configuration should contain unique and stable identifiers for all subnets. The default value is the vlan id, if present.
ref: https://kea.readthedocs.io/en/kea-2.4.0/arm/dhcp4-srv.html#ipv4-subnet-identifier
";
        type = types.int;
        default = config.site.net.${name}.vlan or null;
      };
      start = mkOption {
        description = "First IP address in pool";
        type = types.str;
      };
      end = mkOption {
        description = "Last IP address in pool";
        type = types.str;
      };
      time = mkOption {
        description = "Renew time in seconds";
        type = types.int;
      };
      max-time = mkOption {
        description = "Max renew time in seconds";
        type = types.int;
      };
      router = mkOption {
        description = "The router address for this subnet";
        type = types.str;
      };
      reservations = mkOption {
        type = types.listOf (
          lib.types.submodule {
            options = {
              hostname = lib.mkOption {
                type = lib.types.str;
                description = "The hostname of the client.";
              };
              ip-address = lib.mkOption {
                type = lib.types.str;
                description = "The IP address of the client.";
              };
              hw-address = lib.mkOption {
                type = lib.types.str;
                description = "The MAC address of the client.";
              };
            };
          }
        );
        default = [ ];
        description = "A list of DHCP4 reservations";
      };
      optionData = mkOption {
        type = types.listOf (types.attrsOf types.anything);
        default = [ ];
        description = "Extra dhcp options to be passed directly to kea. They will be merged with the common options by name.";
      };
    };
    #ipv6Router = mkOption {
    #  description = "Who sends router advertisements?";
    #  type = types.nullOr types.str;
    #  default = config.site.net.${name}.dhcp.router or null;
    #};
    domainName = mkOption {
      description = "The domain name for this network";
      type = types.str;
    };
    #extraRecords = import ./record-opts.nix { inherit lib; };
    ddns.enable = mkOption {
      type = types.bool;
      default = false;
      description = "Domain uses dynamic dns to set dhcp client names";
    };
    mtu = mkOption {
      type = types.nullOr types.int;
      default = null;
    };
  };
}
