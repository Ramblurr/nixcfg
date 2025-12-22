{
  lib,
  config,
  ...
}:
let

  inherit (lib)
    mkOption
    types
    ;
in
{ name, ... }:
{
  options = {
    role = mkOption {
      type = types.enum [
        "ap"
        "switch"
        "server"
        "container"
        "client"
      ];
      default = "client";
    };
    #password = mkOption {
    #  type = with types; nullOr str;
    #  default = null;
    #};
    #location = mkOption {
    #  type = with types; nullOr str;
    #  default = null;
    #};
    interfaces = mkOption {
      default = { };
      type = with types; attrsOf (submodule (import ./interface-options.nix { inherit lib config; }));
      description = "Network interfaces";
    };
    physicalInterfaces = mkOption {
      default = lib.filterAttrs (
        _:
        { type, ... }:
        builtins.elem type [
          "phys"
          "veth"
        ]
      ) config.site.hosts.${name}.interfaces;
      type = with types; attrsOf (submodule interfaceOpts);
      description = "Network interfaces that are not virtual";
      readOnly = true;
    };
    isRouter = mkOption {
      type = types.bool;
      default = false;
      description = "Should this host route?";
    };
    firewall = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Enable firewall to disallow incoming connections from core";
      };
      allowedNets = mkOption {
        type = with types; listOf str;
        default = [ ];
        description = "Allow incoming connections from these networks";
      };
    };
    forwardPorts = mkOption {
      type =
        with types;
        listOf (submodule {
          options = {
            proto = mkOption {
              type = types.enum [
                "tcp"
                "udp"
              ];
            };
            sourcePort = mkOption {
              type = types.int;
            };
            destination = mkOption {
              type = types.str;
            };
            reflect = mkOption {
              type = types.bool;
              default = false;
              description = ''
                Enable NAT reflection

                Any forwarded connection will have our static IPv4
                address as source so that forwarded services become
                available internally.

                Unfortunately, this breaks identification by IPv4
                address.
              '';
            };
          };
        });
      default = [ ];
    };
    ospf.stubNets4 = mkOption {
      type = with types; listOf str;
      default = [ ];
      description = "Additional IPv4 networks to announce";
    };
    ospf.stubNets6 = mkOption {
      type = with types; listOf str;
      default = [ ];
      description = "Additional IPv6 networks to announce";
    };
    ospf.allowedUpstreams = mkOption {
      type = with types; listOf str;
      default = [ ];
      description = "Accept default routes from these OSPF routers, in order of preference";
    };
    ospf.allowedUpstreams6 = mkOption {
      type = with types; listOf str;
      default = config.site.hosts.${name}.ospf.allowedUpstreams;
      description = "Accept IPv6 default routes from these OSPF3 routers, in order of preference";
    };
    ospf.upstreamInstance = mkOption {
      type = with types; nullOr int;
      default = null;
      description = "OSPF instance for advertising the default route";
    };
    bgp = mkOption {
      default = null;
      type =
        with types;
        nullOr (submodule {
          options = bgpOpts;
        });
    };
    services.dns = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };

    services.dnscache.enable = mkOption {
      type = types.bool;
      default = false;
    };

    services.monitoring.target = mkOption {
      type = types.listOf types.str;
      default = [ ];
    };

    links = mkOption {
      description = "Which port is connected to what other device? Keys are either network names or known hostnames.";
      default = { };
      type = with types; attrsOf (submodule ((import ./link-options.nix { inherit lib config; }) name));
    };
    wifi = mkOption {
      default = { };
      type =
        with types;
        attrsOf (
          submodule (
            { config, ... }:
            {
              options = {
                band = mkOption {
                  type = enum [
                    "2g"
                    "5g"
                  ];
                  default =
                    if config.channel >= 1 && config.channel <= 14 then
                      "2g"
                    else if config.channel >= 32 && config.channel <= 177 then
                      "5g"
                    else
                      throw "What band is channel ${toString config.channel}?";
                };
                htmode = mkOption {
                  type = enum [
                    "HT20"
                    "HT40-"
                    "HT40+"
                    "HT40"
                    "VHT80"
                  ];
                };
                channel = mkOption {
                  type = int;
                };
                ssids = mkOption {
                  type = attrsOf (
                    submodule (
                      { config, ... }:
                      {
                        options = {
                          net = mkOption {
                            type = str;
                          };
                          psk = mkOption {
                            type = nullOr str;
                            default = null;
                          };
                          hidden = mkOption {
                            type = bool;
                            default = false;
                          };
                          encryption = mkOption {
                            type = enum [
                              "none"
                              "owe"
                              "wpa2"
                              "wpa3"
                            ];
                            default = if config.psk == null then "none" else "wpa3";
                          };
                          mode = mkOption {
                            type = enum [
                              "ap"
                              "sta"
                            ];
                            default = "ap";
                          };
                          ifname = mkOption {
                            type = nullOr str;
                            default = null;
                          };
                          disassocLowAck = mkOption {
                            type = bool;
                            default = true;
                            description = ''
                              Disable for wireless bridges.
                            '';
                          };
                        };
                      }
                    )
                  );
                };
              };
            }
          )
        );
    };
    wifiOnLink.enable = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Install the wifi-on-link.sh script on OpenWRT devices.
      '';
    };
  };
}
