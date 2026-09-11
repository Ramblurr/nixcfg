{ config, lib, ... }:
let
  cfg = config.modules.microvm-guest.hostSecrets;
  hostName = config.networking.hostName;
in
{
  options.modules.microvm-guest.hostSecrets = {
    enable = lib.mkEnableOption "a read-only host-provided MicroVM secrets share";

    source = lib.mkOption {
      type = lib.types.str;
      default = "/run/microvms/secrets/${hostName}";
      readOnly = true;
      description = "Host runtime directory shared with this MicroVM for volatile secrets.";
    };

    mountPoint = lib.mkOption {
      type = lib.types.str;
      default = "/run/host-secrets";
      description = "Guest path where the host-provided secrets share is mounted.";
    };

    tag = lib.mkOption {
      type = lib.types.str;
      default = "host-secrets";
      description = "VirtioFS tag for the host-provided secrets share.";
    };

    services = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Guest systemd services that consume files from the host secrets share.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = lib.hasPrefix "/" cfg.mountPoint;
        message = "The MicroVM host secrets mount point must be absolute.";
      }
      {
        assertion = lib.hasPrefix "/" cfg.source;
        message = "The MicroVM host secrets source must be absolute.";
      }
      {
        assertion = !lib.hasPrefix builtins.storeDir cfg.source;
        message = "The MicroVM host secrets source must not be in the Nix store.";
      }
      {
        assertion = builtins.match "[A-Za-z0-9_.-]+" cfg.tag != null;
        message = "The MicroVM host secrets virtiofs tag contains invalid characters.";
      }
      {
        assertion = lib.all (service: !(lib.hasSuffix ".service" service)) cfg.services;
        message = "MicroVM host secrets consumer service names must omit the .service suffix.";
      }
    ];

    microvm.shares = [
      {
        source = cfg.source;
        mountPoint = cfg.mountPoint;
        tag = cfg.tag;
        proto = "virtiofs";
        readOnly = true;
        cache = "never";
        socket = "${config.modules.microvm-guest.mountBase}/${cfg.tag}.socket";
      }
    ];

    fileSystems.${cfg.mountPoint}.options = [
      "ro"
      "nodev"
      "nosuid"
      "noexec"
    ];

    systemd.services = lib.genAttrs cfg.services (_: {
      unitConfig.RequiresMountsFor = [ cfg.mountPoint ];
    });
  };
}
