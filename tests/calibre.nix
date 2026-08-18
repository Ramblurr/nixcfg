{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  extendedPkgs = import inputs.nixpkgs {
    system = pkgs.stdenv.hostPlatform.system;
    overlays = [ (import ../lib inputs) ];
  };
  evaluated = inputs.nixpkgs.lib.nixosSystem {
    system = pkgs.stdenv.hostPlatform.system;
    specialArgs.lib = extendedPkgs.lib;
    modules = [
      ../modules/services/calibre.nix
      {
        options = {
          modules.services.caddy.protectedRoutes = lib.mkOption {
            type = lib.types.attrs;
            default = { };
          };
          modules.services.caddy.routes = lib.mkOption {
            type = lib.types.attrs;
            default = { };
          };
          modules.zfs.datasets.properties = lib.mkOption {
            type = lib.types.attrs;
            default = { };
          };
          repo.secrets.global.nodes.mali.dataCIDR = lib.mkOption {
            type = lib.types.str;
          };
          site.gatus.endpoints = lib.mkOption {
            type = lib.types.listOf lib.types.attrs;
            default = [ ];
          };
        };
      }
      {
        nixpkgs.pkgs = extendedPkgs;
        system.stateVersion = "26.05";
        boot.loader.grub.devices = [ "nodev" ];
        fileSystems."/" = {
          device = "none";
          fsType = "tmpfs";
        };
        repo.secrets.global.nodes.mali.dataCIDR = "192.0.2.1/24";
        modules.services.calibre = {
          enable = true;
          domain.gui = "calibre.example.test";
          domain.server = "calibre-server.example.test";
          ports.gui = 8080;
          ports.server = 8081;
          mediaNfsShare = "tank2/media";
        };
      }
    ];
  };
  cfg = evaluated.config;
  container = cfg.virtualisation.oci-containers.containers.calibre;
  unit = cfg.systemd.services.podman-calibre;
  serverRoute = cfg.modules.services.caddy.routes.calibre-server;
in
assert
  container.extraOptions == [
    "--health-cmd=curl --fail --silent --show-error --max-time 10 http://127.0.0.1:8081/ >/dev/null"
    "--health-interval=30s"
    "--health-timeout=15s"
    "--health-start-period=5m"
    "--health-retries=3"
    "--health-on-failure=kill"
  ];
assert unit.serviceConfig.Restart == "on-failure";
assert
  serverRoute == {
    publicHost = "calibre-server.example.test";
    upstream = "http://127.0.0.1:8081";
  };
assert
  container.volumes == [
    "/var/lib/calibre:/config:rw"
    "/mnt/mali/tank2/media/books:/media/books:rw"
    "/mnt/downloads:/downloads:rw"
  ];
pkgs.runCommand "calibre-evaluation" { } "touch $out"
