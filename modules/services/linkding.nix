{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.services.linkding;
  httpPort = toString cfg.ports.http;
  dataDir = "/var/lib/linkding";
  homeDir = "/home/${cfg.user.name}";
in
{
  options.modules.services.linkding = {
    enable = lib.mkEnableOption "linkding";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "echo-test.example.com";
      description = "The domain to use for the linkding";
    };
    ports = {
      http = lib.mkOption {
        type = lib.types.port;
        default = 9992;
        description = "The HTTP port to use for the linkding";
      };
    };

    ingress = lib.mkOption {
      type = lib.types.submodule (
        lib.recursiveUpdate (import ./ingress-options.nix { inherit config lib; }) { }
      );
    };
    user = lib.mkOption { type = lib.types.unspecified; };
    group = lib.mkOption { type = lib.types.unspecified; };
  };
  config = lib.mkIf cfg.enable {
    users.users.${cfg.user.name} = {
      name = cfg.user.name;
      uid = lib.mkForce cfg.user.uid;
      isNormalUser = true;
      group = lib.mkForce cfg.group.name;
      home = homeDir;
      linger = true;
      createHome = false;
      autoSubUidGidRange = true;
    };

    users.groups.${cfg.group.name} = {
      name = cfg.group.name;
      gid = lib.mkForce cfg.group.gid;
    };

    modules.zfs.datasets.properties = {
      "tank/svc/linkding"."mountpoint" = dataDir;
    };

    systemd.tmpfiles.rules = [
      "z '${dataDir}' 750 ${cfg.user.name} ${cfg.group.name} - -"
      "d ${homeDir} 750 ${cfg.user.name} ${cfg.group.name} - -"
    ];

    modules.services.postgresql.ensures = [
      {
        username = "linkding";
        databases = [ "linkding" ];
      }
    ];

    home-manager.users.${cfg.user.name} =
      { pkgs, config, ... }:
      let
        homeManagerSessionVars = "";
      in
      {
        imports = [ inputs.quadlet-nix.homeManagerModules.default ];
        home.stateVersion = "21.11";
        home.homeDirectory = homeDir;
        home.packages = [ pkgs.podman ];
        systemd.user.startServices = "sd-switch";
        home.sessionVariables = {
          EDITOR = "vim";
          DBUS_SESSION_BUS_ADDRESS = "unix:path=/run/user/${toString cfg.user.uid}/bus";
          XDG_RUNTIME_DIR = "/run/user/${toString cfg.user.uid}";
        };
        programs.bash = {
          enable = true;
          initExtra = ''
            [[ -f "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh" ]] && source "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"
          '';
        };
        virtualisation.user.quadlet = {
          autoUpdate.enable = true;
          containers = {
            linkding = {
              autoStart = true;
              unitConfig = {
                RequiresMountsFor = [ dataDir ];
              };
              serviceConfig = {
                RestartSec = "10";
                Restart = "always";
              };
              containerConfig = {
                # renovate: docker-image
                image = "docker.io/sissbruecker/linkding:1.37.0";
                autoUpdate = "registry";
                userns = "keep-id";
                publishPorts = [ "127.0.0.1:${httpPort}:9090" ];
                environments = {
                  LD_AUTH_PROXY_USERNAME_HEADER = "HTTP_X_AUTHENTIK_USERNAME";
                  LD_ENABLE_AUTH_PROXY = "True";
                  LD_SUPERUSER_NAME = "casey";
                  LD_DB_ENGINE = "postgres";
                  LD_DB_HOST = "/run/postgresql";
                  LD_DB_PORT = "";
                  LD_DB_DATABASE = "linkding";
                  LD_DB_USER = "linkding";
                  LD_DB_PASSWORD = "";
                };
                podmanArgs = [ ];
                volumes = [
                  "${dataDir}:/etc/linkding/data:rw"
                  "/run/postgresql:/run/postgresql:ro"
                ];
              };
            };
          };
        };
      };

    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [ cfg.domain ];
      };
    };
    modules.services.ingress.virtualHosts.${cfg.domain} = {
      acmeHost = cfg.ingress.domain;
      upstream = "http://127.0.0.1:${httpPort}";
      forwardAuth = true;
    };
  };
}
