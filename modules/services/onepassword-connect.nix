{
  options,
  config,
  lib,
  utils,
  pkgs,
  inputs,
  unstable,
  mine,
  ...
}:
let
  cfg = config.modules.services.onepassword-connect;
  home-ops = config.repo.secrets.home-ops;
  dataDir = "/var/lib/onepassword-connect";
in
{
  options.modules.services.onepassword-connect = {
    enable = lib.mkEnableOption "onepassword-connect";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "onepassword-connect.example.com";
      description = "The domain to use for the onepassword-connect";
    };

    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
    };
    ports = {
      api = lib.mkOption { type = lib.types.port; };
      sync = lib.mkOption { type = lib.types.port; };
    };
    subnet = lib.mkOption {
      type = lib.types.str;
      example = "10.123.123.1/29";
    };
    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption { type = lib.types.unspecified; };
  };

  config = lib.mkIf cfg.enable {

    users.users.${cfg.user.name} = {
      name = cfg.user.name;
      uid = lib.mkForce cfg.user.uid;
      isSystemUser = true;
      group = lib.mkForce cfg.group.name;
      home = "/var/lib/onepassword-connect";
      createHome = false;
      autoSubUidGidRange = true;
    };

    users.groups.${cfg.group.name} = {
      name = cfg.group.name;
      gid = lib.mkForce cfg.group.gid;
    };

    system.activationScripts = {
      enableLingering = ''
        touch /var/lib/systemd/linger/${cfg.user.name}
      '';
    };

    systemd.tmpfiles.rules = [ "d ${dataDir} 0770 ${cfg.user.name} ${cfg.group.name}" ];

    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/onepassword-connect"."mountpoint" = "${dataDir}";
      "rpool/encrypted/safe/svc/onepassword-connect"."com.sun:auto-snapshot" = "false";
    };

    modules.services.ingress.virtualHosts.${cfg.domain} = {
      acmeHost = cfg.ingress.domain;
      upstream = "http://127.0.0.1:${toString cfg.ports.api}";
    };

    virtualisation.quadlet = {
      containers = {
        op-connect-api = {
          autoStart = true;
          unitConfig = {
            RequiresMountsFor = [ dataDir ];
          };
          serviceConfig = {
            RestartSec = "10";
            Restart = "always";
          };
          containerConfig = {
            # renovate: datasource=docker depName=docker.io/1password/connect-api
            image = "docker.io/1password/connect-api:1.7.2";
            environments = {
              XDG_DATA_HOME = "/config";
              HOME = "/config";
            };
            podmanArgs = [ "--user ${toString cfg.user.uid}" ];
            publishPorts = [ "127.0.0.1:${toString cfg.ports.api}:8080" ];
            volumes = [ "${dataDir}:/config:rw" ];
            networks = [ "onepassword-connect.network" ];
          };
        };
        op-connect-sync = {
          autoStart = true;
          unitConfig = {
            RequiresMountsFor = [ dataDir ];
          };
          serviceConfig = {
            RestartSec = "10";
            Restart = "always";
          };
          containerConfig = {
            # renovate: datasource=docker depName=docker.io/1password/connect-sync
            image = "docker.io/1password/connect-sync:1.7.2";
            environments = {
              XDG_DATA_HOME = "/config";
              HOME = "/config";
            };
            podmanArgs = [ "--user ${toString cfg.user.uid}" ];
            publishPorts = [ "127.0.0.1:${toString cfg.ports.sync}:8080" ];
            volumes = [ "${dataDir}:/config:rw" ];
            networks = [ "onepassword-connect.network" ];
          };
        };
      };
      networks = {
        onepassword-connect.networkConfig.subnets = [ cfg.subnet ];
      };
    };
  };
}
