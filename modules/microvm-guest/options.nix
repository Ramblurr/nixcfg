{
  config,
  lib,
  ...
}:

let
  inherit (lib)
    mkIf
    mkOption
    mkEnableOption
    types
    ;
  inherit (config.networking) hostName;
in
{
  options.modules.microvm-guest = {
    enable = mkEnableOption "Enable if this host is a micrvm guest";
    host = mkOption {
      type = types.enum [
        "quine"
        "dewey"
      ];
      default = null;
      description = ''
        Server that is supposed to host this MicroVM.
      '';
    };

    hostFQDN = mkOption {
      description = ''
        FQDN of the host that is supposed to host this MicroVM.
      '';
    };

    autoNetSetup = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Automatically configure MicroVM network interfaces and
        systemd-networkd according to our network data.
      '';
    };

    mounts = mkOption {
      description = "Persistent filesystems to create, without leading /.";
      type = types.listOf types.str;
      default = [ ];
    };

    mountBase = mkOption {
      description = "Location (ZFS dataset, ...) where all the shares live.";
      type = types.path;
      default = "/var/lib/microvms/${hostName}";
    };

    nixStoreBackend = mkOption {
      type = types.enum [
        "virtiofs"
        "blk"
      ];
      default = "virtiofs";
      description = ''
        /nix/store via virtiofs from the host, or create
        an erofs block image for it.
      '';
    };

    homeManager = {
      enable = mkEnableOption "Enable home-manager for the microvm";
      username = mkOption {
        type = types.str;
        description = ''
          Username for the home-manager configuration.
        '';
      };
      uid = mkOption {
        type = types.int;
        default = null;
        description = ''
          UID for the home-manager configuration.
        '';
      };
      gid = mkOption {
        type = types.int;
        default = null;
        description = ''
          GID for the home-manager configuration.
        '';
      };
    };

    quadlet = {
      enable = mkEnableOption "Enable podman quadlet for the microvm";
    };

  };
}
