{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.services.archivebox;
  service = "archivebox";
  dataDir = "/var/lib/${service}";
  homeDir = "/home/${cfg.user.name}";
in
{
  options.modules.services.archivebox = {
    enable = lib.mkEnableOption "archivebox";
    domain = lib.mkOption {
      type = lib.types.str;
      example = "archive.example.com";
      description = "The domain to use";
    };
    ports = {
      http = lib.mkOption {
        type = lib.types.port;
        description = "The HTTP port to use";
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
      "tank/svc/${service}"."mountpoint" = dataDir;
    };

    systemd.tmpfiles.rules = [
      "z '${dataDir}' 750 ${cfg.user.name} ${cfg.group.name} - -"
      "d '${dataDir}/data' 750 ${cfg.user.name} ${cfg.group.name} - -"
      "d '${dataDir}/chrome-profile' 750 ${cfg.user.name} ${cfg.group.name} - -"
      "d ${homeDir} 750 ${cfg.user.name} ${cfg.group.name} - -"
    ];

    modules.services.ingress.domains = lib.mkIf cfg.ingress.external {
      "${cfg.ingress.domain}" = {
        externalDomains = [ cfg.domain ];
      };
    };
    modules.services.ingress.virtualHosts.${cfg.domain} = {
      acmeHost = cfg.ingress.domain;
      upstream = "http://127.0.0.1:${toString cfg.ports.http}";
      forwardAuth = true;
    };

    home-manager.users.${cfg.user.name} =
      { pkgs, config, ... }:
      {
        imports = [ inputs.quadlet-nix.homeManagerModules.default ];
        home.stateVersion = "21.11";
        home.homeDirectory = homeDir;
        home.packages = [ pkgs.podman ];
        home.sessionVariables = {
          EDITOR = "vim";
          DBUS_SESSION_BUS_ADDRESS = "unix:path=/run/user/${cfg.user.uid}/bus";
        };
        systemd.user.startServices = "sd-switch";
        programs.bash.enable = true;
        home.sessionVariables.XDG_RUNTIME_DIR = "/run/user/${toString cfg.user.uid}";
        virtualisation.user.quadlet = {
          autoUpdate.enable = true;
          containers =
            let
              sharedContainerConfig = {
                # renovate: docker-image
                image = "ghcr.io/archivebox/archivebox/archivebox:dev";
                autoUpdate = "registry";
                userns = "keep-id:uid=${toString cfg.user.uid},gid=${toString cfg.group.gid}";
                environments = {
                  REVERSE_PROXY_USER_HEADER = "X-authentik-username";
                  REVERSE_PROXY_WHITELIST = "10.0.0.0/16";
                  PUBLIC_ADD_VIEW = "True"; # needed for firefox extension
                  ALLOWED_HOSTS = "*";
                  PUBLIC_INDEX = "False";
                  PUBLIC_SNAPSHOTS = "False";
                  MEDIA_MAX_SIZE = "1024m";
                  #CHROME_USER_DATA_DIR = "None";
                  #CHROME_USER_DATA_DIR = "/chrome-profile";
                  # these two lines a workaround for chromium crashing with --headless=new
                  XDG_CONFIG_HOME = "/tmp/xdg";
                  XDG_CACHE_HOME = "/tmp/xdg";
                };
                podmanArgs = [
                  "--entrypoint /usr/local/bin/python"
                  "--passwd"
                  "--passwd-entry archivebox:x:${toString cfg.user.uid}:${toString cfg.group.gid}::/home/archivebox:/bin/sh"
                ];
                volumes = [
                  "${dataDir}/data:/data:rw"
                  "${dataDir}/chrome-profile:/chrome-profile:rw"
                ];
              };
            in
            {
              archivebox-scheduler = {
                autoStart = true;
                unitConfig = {
                  RequiresMountsFor = [ dataDir ];
                };
                serviceConfig = {
                  RestartSec = "10";
                  Restart = "always";
                };
                containerConfig = sharedContainerConfig // {
                  exec = "/usr/local/bin/archivebox schedule --foreground";
                };
              };
              archivebox = {
                autoStart = true;
                unitConfig = {
                  RequiresMountsFor = [ dataDir ];
                };
                serviceConfig = {
                  RestartSec = "10";
                  Restart = "always";
                };
                containerConfig = sharedContainerConfig // {
                  exec = "/usr/local/bin/archivebox server --quick-init 0.0.0.0:8000";
                  publishPorts = [ "127.0.0.1:${toString cfg.ports.http}:8000" ];
                };
              };
            };
        };
      };
  };
}
