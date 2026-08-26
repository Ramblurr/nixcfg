{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  evaluated = inputs.nixpkgs.lib.nixosSystem {
    system = pkgs.stdenv.hostPlatform.system;
    modules = [
      ../modules/zfs-attrs.nix
      ../modules/services/grafana.nix
      {
        options.repo.secrets = lib.mkOption { type = lib.types.attrs; };
      }
      {
        nixpkgs.pkgs = pkgs;
        system.stateVersion = "26.05";
        boot.loader.grub.devices = [ "nodev" ];
        fileSystems."/" = {
          device = "none";
          fsType = "tmpfs";
        };
        repo.secrets.global.email = {
          siteRelay = "mail.example.test";
          home = "grafana@example.test";
        };
        modules.zfs.datasets.enable = true;
        modules.services.grafana = {
          enable = true;
          domain = "grafana.example.test";
        };
      }
    ];
  };
  service = evaluated.config.systemd.services.grafana;
  dependencyNames = service.requires ++ service.wants ++ service.after ++ service.bindsTo;
in
assert !(builtins.elem "var-lib-grafana.mount" dependencyNames);
assert builtins.elem "zfs-datasets.service" service.requires;
assert builtins.elem "zfs-datasets.service" service.after;
assert builtins.elem "zfs-mount.service" service.bindsTo;
assert service.unitConfig.AssertPathIsMountPoint == [ "/var/lib/grafana" ];
assert service.unitConfig.RequiresMountsFor == [ "/var/lib/grafana" ];
pkgs.runCommand "grafana-zfs-readiness-evaluation" { } "touch $out"
