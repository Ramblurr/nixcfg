{
  options,
  config,
  lib,
  utils,
  pkgs,
  inputs,
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
      linger = true;
      createHome = false;
      autoSubUidGidRange = true;
    };

    users.groups.${cfg.group.name} = {
      name = cfg.group.name;
      gid = lib.mkForce cfg.group.gid;
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

    systemd.services."podman-create-op-connect" =
      let
        podName = "op-connect";
        ports = [
          "127.0.0.1:${toString cfg.ports.api}:8080"
          "127.0.0.1:${toString cfg.ports.sync}:8081"
        ];
        portsMapping = lib.concatMapStrings (port: " -p " + port) ports;
      in
      {
        path = [
          pkgs.coreutils
          config.virtualisation.podman.package
        ];
        script = ''
          podman pod exists ${podName} || podman pod create -n ${podName} ${portsMapping} --dns ${builtins.head config.repo.secrets.global.nameservers}
        '';

        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = "yes";
          ExecStop = "podman pod rm -i -f ${podName}";
        };
      };
    virtualisation.quadlet = {
      autoEscape = true;
      containers = {
        op-connect-api = {
          autoStart = true;
          unitConfig = {
            RequiresMountsFor = [ dataDir ];
            After = [ "podman-create-op-connect.service" ];
            Requires = [ "podman-create-op-connect.service" ];
          };
          serviceConfig = {
            RestartSec = "10";
            Restart = "always";
          };
          containerConfig = {
            # renovate: docker-image
            image = "docker.io/1password/connect-api:1.7.3";
            environments = {
              XDG_DATA_HOME = "/config";
              OP_BUS_PORT = "11220";
              OP_BUS_PEERS = "localhost:11221";
              OP_SESSION = "/config/1password-credentials.json";
            };
            podmanArgs = [
              "--user ${toString cfg.user.uid}"
              "--pod op-connect"
            ];
            volumes = [ "${dataDir}:/config:rw" ];
          };
        };
        op-connect-sync = {
          autoStart = true;
          unitConfig = {
            RequiresMountsFor = [ dataDir ];
          };
          serviceConfig = {
            RestartSec = "2";
            Restart = "always";
          };
          containerConfig = {
            # renovate: docker-image
            image = "docker.io/1password/connect-sync:1.7.3";
            environments = {
              XDG_DATA_HOME = "/config";
              OP_BUS_PORT = "11221";
              OP_HTTP_PORT = "8081";
              OP_BUS_PEERS = "localhost:11220";
              OP_SESSION = "/config/1password-credentials.json";
            };
            podmanArgs = [
              "--user ${toString cfg.user.uid}"
              "--pod op-connect"
            ];
            volumes = [ "${dataDir}:/config:rw" ];
          };
        };
      };
    };
  };
}
