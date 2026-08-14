{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.pocket-id;
  dataDir = "/var/lib/pocket-id";

  mkContainer = instance: {
    autoStart = true;
    privateNetwork = false;
    bindMounts = {
      ${dataDir} = {
        hostPath = instance.hostPath;
        isReadOnly = false;
      };
      "/run/secrets/pocket-id-encryption-key" = {
        hostPath = config.sops.secrets.${instance.sopsKey}.path;
        isReadOnly = true;
      };
    };
    config =
      { ... }:
      {
        nixpkgs.pkgs = pkgs;
        system.stateVersion = config.system.stateVersion;
        networking.firewall.enable = false;
        users.users.pocket-id = {
          isSystemUser = true;
          uid = 988;
          group = "pocket-id";
          createHome = false;
        };
        users.groups.pocket-id.gid = 987;
        services.pocket-id = {
          enable = true;
          credentials.ENCRYPTION_KEY = "/run/secrets/pocket-id-encryption-key";
          settings = {
            APP_URL = "https://${instance.publicDomain}";
            TRUST_PROXY = true;
            HOST = "127.0.0.1";
            PORT = instance.port;
          };
        };
      };
  };
in
{
  options.modules.services.pocket-id.instances = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule {
        options = {
          containerName = lib.mkOption {
            type = lib.types.str;
            description = "NixOS container name.";
          };
          publicDomain = lib.mkOption {
            type = lib.types.str;
            description = "Public Pocket ID domain.";
          };
          port = lib.mkOption {
            type = lib.types.port;
            description = "Loopback port shared with the host.";
          };
          dataset = lib.mkOption {
            type = lib.types.str;
            description = "ZFS dataset storing Pocket ID data.";
          };
          hostPath = lib.mkOption {
            type = lib.types.str;
            description = "Host dataset mountpoint.";
          };
          sopsKey = lib.mkOption {
            type = lib.types.str;
            description = "SOPS encryption-key name.";
          };
        };
      }
    );
    default = { };
    description = "Declarative Pocket ID instances.";
  };

  config = lib.mkIf (cfg.instances != { }) {
    assertions = [
      {
        assertion =
          lib.length (lib.unique (map (instance: instance.containerName) (lib.attrValues cfg.instances)))
          == lib.length (lib.attrValues cfg.instances);
        message = "Pocket ID instances must use distinct container names";
      }
      {
        assertion =
          lib.length (lib.unique (map (instance: instance.port) (lib.attrValues cfg.instances)))
          == lib.length (lib.attrValues cfg.instances);
        message = "Pocket ID instances must use distinct ports";
      }
    ];

    modules.zfs.datasets.properties = lib.mapAttrs' (
      _: instance:
      lib.nameValuePair instance.dataset {
        mountpoint = instance.hostPath;
        "com.sun:auto-snapshot" = "false";
      }
    ) cfg.instances;

    sops.secrets = lib.mapAttrs' (
      _: instance:
      lib.nameValuePair instance.sopsKey {
        restartUnits = [ "container@${instance.containerName}.service" ];
      }
    ) cfg.instances;

    containers = lib.mapAttrs' (
      _: instance: lib.nameValuePair instance.containerName (mkContainer instance)
    ) cfg.instances;

    systemd.services = lib.mapAttrs' (
      _: instance:
      lib.nameValuePair "container@${instance.containerName}" {
        requires = [
          "sops-install-secrets.service"
          "zfs-datasets.service"
        ];
        after = [
          "sops-install-secrets.service"
          "zfs-datasets.service"
        ];
        unitConfig.RequiresMountsFor = [
          instance.hostPath
          config.sops.secrets.${instance.sopsKey}.path
        ];
      }
    ) cfg.instances;
  };
}
