{
  lib,
  options,
  ...
}:
let
  inherit (lib)
    mkOption
    types
    ;

  nodesType = types.attrsOf (
    types.submodule (_hostSubmod: {
      options = {
        mgmtCIDR = mkOption {
          type = types.nullOr types.str;
          default = null;
        };
        dataCIDR = mkOption {
          type = types.nullOr types.str;
          default = null;
        };
        primCIDR = mkOption {
          type = types.nullOr types.str;
          default = null;
        };
        vlanPrimaryEnabled = mkOption {
          type = types.bool;
          default = false;
        };
        mgmtIface = mkOption {
          type = types.nullOr types.str;
          default = null;
        };
        dataIface = mkOption {
          type = types.nullOr types.str;
          default = null;
        };
        zreplSource = mkOption {
          type = types.bool;
          default = false;
        };

      };
    })
  );

in
{
  options = {
    globals = mkOption {
      default = { };
      type = types.submodule {
        options = {
          homeAssistantUrlExternal = mkOption { type = types.str; };
          localAtticSubstituter = mkOption { type = types.str; };
          localAtticPublicKey = mkOption { type = types.str; };
          pubKeys = mkOption {
            type = types.listOf types.str;
            description = "My ssh pub keys";
          };
          nameservers = mkOption {
            type = types.listOf types.str;
            description = "my home ns";
          };

          wifi = mkOption {
            type = types.submodule {
              options = {
                ssid = mkOption {
                  type = types.str;
                  description = "The SSID of the network";
                };
                password = mkOption {
                  type = types.str;
                  description = "The password of the network";
                };
              };
            };
          };
          email = mkOption {
            type = types.attrsOf types.str;
          };
          domain = mkOption {
            type = types.attrsOf types.str;
          };
          mgmtGateway = mkOption {
            type = types.str;
          };
          nodes = mkOption {
            type = nodesType;
          };
        };
      };
    };

    _globalsDefs = mkOption {
      type = types.unspecified;
      default = options.globals.definitions;
      readOnly = true;
      internal = true;
    };
  };
}
