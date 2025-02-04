{
  config,
  pkgs,
  lib,
  ...
}:

let
  # Define the submodule for each relay instance
  relayOptions =
    types:
    { name, config, ... }:
    {
      options = {
        port = lib.mkOption {
          type = types.int;
          example = 5353;
          description = "The UDP port to listen to.";
        };
        id = lib.mkOption {
          type = types.int;
          description = "Unique ID for the relay instance.";
        };
        interfaces = lib.mkOption {
          type = types.listOf types.str;
          example = [
            "eth0"
            "eth1"
          ];
          description = "List of interfaces to listen on and forward packets to.";
        };
        multicast = lib.mkOption {
          type = types.nullOr types.str;
          default = null;
          description = "Optional multicast group to relay.";
        };
        blockCidr = lib.mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = "List of CIDRs to block packets from.";
        };
        allowCidr = lib.mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = "List of CIDRs to allow packets from.";
        };
        msearch = lib.mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = "M-SEARCH options for SSDP or similar protocols.";
        };
      };
    };

  cfg = config.services.udpbroadcastrelay;
in
{
  options.services.udpbroadcastrelay = {
    enable = lib.mkEnableOption "UDP Broadcast Relay service";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.udpbroadcastrelay;
      description = "The package containing the udpbroadcastrelay.";
    };

    # This defines the attribute set for multiple instances
    # Each key in the set will be a named instance (like 'syncthing' or 'roon')
    # The value for each key follows the relayOptions structure
    instances = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule (relayOptions lib.types));
      default = { };
      description = "Attribute set of UDP broadcast relay instances.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = lib.mkIf (cfg.package == null) [
      {
        assertion = false;
        message = "services.udpbroadcastrelay.package must be set to a valid package containing the udpbroadcastrelay binary.";
      }
    ];

    # Generate a systemd service for each instance
    systemd.services = lib.mapAttrs' (name: instanceCfg: {
      name = "udpbroadcastrelay-${name}";
      value = {
        description = "UDP Broadcast Relay Service (${name})";
        after = [ "network-online.target" ];
        wants = [ "network-online.target" ];
        serviceConfig = {
          ExecStart = ''
            ${cfg.package}/bin/udpbroadcastrelay \
            --id ${toString instanceCfg.id} \
            --port ${toString instanceCfg.port} \
            ${lib.concatStringsSep " " (map (dev: "--dev ${dev}") instanceCfg.interfaces)} \
            ${lib.optionalString (instanceCfg.multicast != null) "--multicast ${instanceCfg.multicast}"} \
            ${lib.concatStringsSep " " (map (cidr: "--blockcidr ${cidr}") instanceCfg.blockCidr)} \
            ${lib.concatStringsSep " " (map (cidr: "--allowcidr ${cidr}") instanceCfg.allowCidr)} \
            ${lib.concatStringsSep " " (map (m: "--msearch ${m}") instanceCfg.msearch)}
          '';
          Restart = "always";
          User = "root";
        };
        wantedBy = [ "multi-user.target" ];
      };
    }) cfg.instances;
  };
}
