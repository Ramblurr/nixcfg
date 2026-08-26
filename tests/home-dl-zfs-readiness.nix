{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  hostPkgs = import inputs.nixpkgs {
    system = pkgs.stdenv.hostPlatform.system;
    config.allowUnfree = true;
    overlays = [
      (import ../lib inputs)
      (import ../overlays/nixpkgs-mine-packages.nix inputs)
    ];
  };
  evaluated = inputs.nixpkgs.lib.nixosSystem {
    system = pkgs.stdenv.hostPlatform.system;
    specialArgs.lib = hostPkgs.lib;
    modules = [
      ../modules/services/home-dl.nix
      {
        disabledModules = [ ../modules/services/home-dl/qbittorrent.nix ];
        options = {
          modules.networking.systemd-netns-private = lib.mkOption {
            type = lib.types.attrs;
            default = { };
          };
          modules.services.caddy.protectedRoutes = lib.mkOption {
            type = lib.types.attrs;
            default = { };
          };
          modules.zfs.datasets.properties = lib.mkOption {
            type = lib.types.attrs;
            default = { };
          };
          repo.secrets = lib.mkOption { type = lib.types.attrs; };
          site.gatus = lib.mkOption {
            type = lib.types.attrs;
            default = { };
          };
        };
      }
      {
        nixpkgs.pkgs = hostPkgs;
        system.stateVersion = "26.05";
        boot.loader.grub.devices = [ "nodev" ];
        fileSystems."/" = {
          device = "none";
          fsType = "tmpfs";
        };
        repo.secrets = {
          global.nodes.mali.dataCIDR = "192.0.2.1/24";
          home-ops = {
            users.media.name = "media";
            groups.media.name = "media";
          };
        };
        site.gatus.groups.media = "Media";
        modules.services.home-dl = {
          enable = true;
          baseDomain = "example.test";
          mediaNfsShare = "tank2/media";
          subnet = {
            hostAddr = "192.0.2.1/30";
            nsAddr = "192.0.2.2/30";
          };
        };
      }
    ];
  };
  services = evaluated.config.systemd.services;
  consumers = map (name: services.${name}) [
    "sonarr"
    "radarr"
    "sabnzbd"
    "prowlarr"
  ];
  dependencyNames = service: service.requires ++ service.wants ++ service.after ++ service.bindsTo;
  nfsMount = "mnt-mali-tank2-media.mount";
  nativeZfsMounts = [
    "mnt-downloads.mount"
    "var-lib-private-home\\x2ddl.mount"
  ];
  hasReadiness =
    service:
    builtins.elem "zfs-datasets.service" service.requires
    && builtins.elem "zfs-datasets.service" service.after
    && builtins.elem "zfs-mount.service" service.bindsTo;
in
assert lib.all hasReadiness consumers;
assert lib.all (
  service: lib.all (mount: !(builtins.elem mount (dependencyNames service))) nativeZfsMounts
) consumers;
assert lib.all (
  service: builtins.elem nfsMount service.after && builtins.elem nfsMount service.bindsTo
) consumers;
assert lib.all (
  service:
  service.unitConfig.AssertPathIsMountPoint == [
    "/var/lib/private/home-dl"
    "/mnt/downloads"
  ]
) consumers;
assert lib.all (
  service:
  lib.all (path: builtins.elem path service.unitConfig.RequiresMountsFor) [
    "/var/lib/private/home-dl"
    "/mnt/downloads"
  ]
) consumers;
pkgs.runCommand "home-dl-zfs-readiness-evaluation" { } "touch $out"
