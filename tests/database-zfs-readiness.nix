{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  evaluated = inputs.nixpkgs.lib.nixosSystem {
    system = pkgs.stdenv.hostPlatform.system;
    modules = [
      ../modules/zfs-attrs.nix
      ../modules/services/mariadb.nix
      ../modules/services/postgresql.nix
      {
        options = {
          modules.services.onepassword-systemd-credentials = {
            enable = lib.mkEnableOption "test provider";
            consumers = lib.mkOption {
              type = lib.types.attrs;
              default = { };
            };
          };
          site.net.mgmt.hosts4 = lib.mkOption { type = lib.types.attrs; };
        };
      }
      {
        nixpkgs.pkgs = pkgs;
        networking.hostName = "dewey";
        site.net.mgmt.hosts4 = {
          dewey = [ "192.0.2.14" ];
          mali = [ "192.0.2.3" ];
        };
        system.stateVersion = "26.05";
        boot.loader.grub.devices = [ "nodev" ];
        fileSystems."/" = {
          device = "none";
          fsType = "tmpfs";
        };
        modules.zfs.datasets.enable = true;
        modules.services.mariadb = {
          enable = true;
          package = pkgs.mariadb_114;
        };
        modules.services.postgresql.enable = true;
      }
    ];
  };
  mysql = evaluated.config.systemd.services.mysql;
  postgresql = evaluated.config.systemd.services.postgresql;
  dependencyNames = service: service.requires ++ service.wants ++ service.after ++ service.bindsTo;
  hasZfsReadiness =
    service:
    builtins.elem "zfs-datasets.service" service.requires
    && builtins.elem "zfs-datasets.service" service.after
    && builtins.elem "zfs-mount.service" service.bindsTo;
in
assert hasZfsReadiness mysql;
assert hasZfsReadiness postgresql;
assert !(builtins.elem "var-lib-mysql.mount" (dependencyNames mysql));
assert !(builtins.elem "var-lib-postgresql.mount" (dependencyNames postgresql));
assert mysql.unitConfig.AssertPathIsMountPoint == [ "/var/lib/mysql" ];
assert builtins.elem "/var/lib/mysql" mysql.unitConfig.RequiresMountsFor;
assert postgresql.unitConfig.AssertPathIsMountPoint == [ "/var/lib/postgresql" ];
assert builtins.elem "/var/lib/postgresql" postgresql.unitConfig.RequiresMountsFor;
pkgs.runCommand "database-zfs-readiness-evaluation" { } "touch $out"
