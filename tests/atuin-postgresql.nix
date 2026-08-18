{
  inputs,
  pkgs,
}:
let
  lib = inputs.nixpkgs.lib;
  testOptions =
    { lib, ... }:
    {
      options = {
        modules.services.caddy.routes = lib.mkOption {
          type = lib.types.attrs;
          default = { };
        };
        modules.services.onepassword-systemd-credentials = lib.mkOption {
          type = lib.types.attrs;
          default = {
            enable = false;
          };
        };
        modules.zfs.datasets.properties = lib.mkOption {
          type = lib.types.attrsOf (lib.types.attrsOf lib.types.str);
          default = { };
        };
        site.gatus = lib.mkOption {
          type = lib.types.attrs;
          default = { };
        };
      };
    };
  cfg =
    (lib.nixosSystem {
      specialArgs = { inherit inputs; };
      modules = [
        ../modules/services/postgresql.nix
        ../modules/services/atuin-sync.nix
        testOptions
        {
          nixpkgs.pkgs = pkgs;
          networking.hostName = "dewey";
          system.stateVersion = "26.05";
          boot.loader.grub.devices = [ "nodev" ];
          fileSystems."/" = {
            device = "none";
            fsType = "tmpfs";
          };
          site.gatus.groups.home = "Home";
          modules.services.postgresql.enable = true;
          modules.services.atuin-sync = {
            enable = true;
            domain = "atuin.example.test";
            ports.http = 10011;
          };
        }
      ];
    }).config;
  service = cfg.systemd.services.atuin;
in
assert builtins.elem "postgresql.service" service.requires;
assert builtins.elem "postgresql.service" service.after;
pkgs.runCommand "atuin-postgresql-ordering" { } "touch $out"
