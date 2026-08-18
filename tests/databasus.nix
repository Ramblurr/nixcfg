{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  evaluated = inputs.nixpkgs.lib.nixosSystem {
    system = pkgs.stdenv.hostPlatform.system;
    modules = [
      ../modules/services/databasus
      ../modules/services/podman.nix
      ../modules/zfs-attrs.nix
      {
        options = {
          modules.boot.zfs.rootPool = lib.mkOption {
            type = lib.types.str;
            default = "rpool2";
          };
          modules.services.caddy.routes = lib.mkOption {
            type = lib.types.attrs;
            default = { };
          };
        };
      }
      {
        system.stateVersion = "26.05";
        boot.loader.grub.devices = [ "nodev" ];
        fileSystems."/" = {
          device = "none";
          fsType = "tmpfs";
        };
        modules.services.databasus = {
          enable = true;
          domain = "databasus.example.test";
          allowedRemoteIPs = [
            "192.0.2.0/24"
            "100.64.0.0/10"
          ];
        };
      }
    ];
  };
  cfg = evaluated.config;
  service = cfg.modules.services.databasus;
  container = cfg.virtualisation.oci-containers.containers.databasus;
  unit = cfg.systemd.services.podman-databasus;
  route = cfg.modules.services.caddy.routes.databasus;
in
assert
  service.image
  == "docker.io/databasus/databasus:v3.53.0@sha256:ed017f58674a18173a924cc7fd8059121adbd0aa6171d68357c4398fa14e0763";
assert cfg.modules.services.podman.enable;
assert cfg.virtualisation.oci-containers.backend == "podman";
assert cfg.modules.zfs.datasets.enable;
assert
  cfg.modules.zfs.datasets.properties.${service.dataset} == {
    atime = "off";
    compression = "zstd";
    mountpoint = service.dataDir;
  };
assert container.autoStart;
assert container.ports == [ "127.0.0.1:4005:4005" ];
assert container.volumes == [ "/var/lib/databasus:/databasus-data:rw" ];
assert
  container.environment == {
    DATABASUS_URL = "https://databasus.example.test";
    IS_DISABLE_ANONYMOUS_TELEMETRY = "true";
  };
assert builtins.elem "--health-cmd=databasus healthcheck" container.extraOptions;
assert builtins.elem "zfs-datasets.service" unit.requires;
assert builtins.elem "zfs-datasets.service" unit.after;
assert unit.unitConfig.RequiresMountsFor == [ "/var/lib/databasus" ];
assert
  route == {
    publicHost = "databasus.example.test";
    upstream = "http://127.0.0.1:4005";
    allowedRemoteIPs = [
      "192.0.2.0/24"
      "100.64.0.0/10"
    ];
  };
pkgs.runCommand "databasus-evaluation" { } "touch $out"
