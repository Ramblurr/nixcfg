{ lib, config, ... }:
let

  inherit (lib)
    concatMap
    elemAt
    mkOption
    types
    ;
  upstreamOpts = {

    provider = mkOption {
      type = types.str;
    };
    link = mkOption {
      type = with types; nullOr str;
      default = null;
      description = "Underlying interface name for eg. PPPoE";
    };
    staticIpv4Address = mkOption {
      type = with types; nullOr str;
      default = null;
    };
    upBandwidth = mkOption {
      type = with types; nullOr int;
      default = null;
    };
    noNat.subnets4 = mkOption {
      type = with types; listOf str;
      default = [ ];
      description = "Do not NAT traffic from these public static subnets";
    };
    noNat.subnets6 = mkOption {
      type = with types; listOf str;
      default = [ ];
      description = "Do not NAT66 traffic from these public static subnets";
    };
    user = mkOption {
      type = with types; nullOr str;
      default = null;
    };
    password = mkOption {
      type = with types; nullOr str;
      default = null;
    };
  };
in
{ ... }:
{
  options = {
    hwaddr = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Static MAC address";
    };
    type = mkOption {
      type = types.enum [
        "phys"
        "veth"
        "pppoe"
        "bridge"
        "wireguard"
        "vxlan"
        "gre"
      ];
      description = ''
        - veth: Virtual ethernet to be attached to a bridge.
        - phys: (Physical) types.interface from a server
      '';
    };
    gw4 = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "IPv4 gateway";
    };
    gw6 = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "IPv6 gateway";
    };
    routes = mkOption {
      type = types.listOf (types.attrsOf types.anything);
      default = [ ];
    };
    routingPolicyRules = mkOption {
      type = types.listOf (types.attrsOf types.anything);
      default = [ ];
    };
    upstream = mkOption {
      type =

        types.nullOr (
          types.submodule {
            options = upstreamOpts;
          }
        );
      default = null;
      description = "Upstream types.interface configuration";
    };
    mtu = mkOption {
      type = types.nullOr types.int;
      default = null;
    };
    wireguard = mkOption {
      default = null;
      type =

        types.nullOr (
          types.submodule {
            options = {
              endpoint = mkOption {
                type = types.str;
              };
              publicKey = mkOption {
                type = types.str;
              };
              privateKey = mkOption {
                type = types.str;
              };
              addresses = mkOption {
                type = types.listOf types.str;
              };
            };
          }
        );
    };
    gre = mkOption {
      default = null;
      type = types.nullOr (
        types.submodule {
          options = {
            local = mkOption { type = types.str; };
            remote = mkOption { type = types.str; };
          };
        }
      );
    };
    vxlan = mkOption {
      default = null;
      type =

        types.nullOr (
          types.submodule {
            options = {
              peer = mkOption {
                type = types.str;
              };
            };
          }
        );
    };
  };
}
