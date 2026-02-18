{
  config,
  lib,
  ...
}:

let
  inherit (lib)
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

    bootstrapSops = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Bootstrap sops-nix with a systemd credential.
        '';
      };
      credentialHostPath = mkOption {
        type = types.str;
        default = "/run/secrets/microvm-${hostName}-sops-key";
        description = "";
      };
    };
    writableStoreOverlay.enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Enable writable /nix/store overlay for the MicroVM.
      '';
    };

    autoNetSetup.enable = mkOption {
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
    devSandbox = {
      enable = mkEnableOption "Enable dev sandbox for the microvm";
      username = mkOption {
        type = types.str;
        default = "ramblurr";
        description = ''
          Username for the dev sandbox configuration.
        '';
      };
      sharedDirs = mkOption {
        type = types.oneOf [
          types.str
          types.anything
        ];
        default = [
          "/home/ramblurr/.claude"
        ];
        description = ''
          Directories to share with the dev sandbox.
          They will be mounted under the same path inside the vm.
        '';
      };

    };
  };
}
