{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  evaluated = inputs.nixpkgs.lib.nixosSystem {
    system = pkgs.stdenv.hostPlatform.system;
    modules = [
      inputs.quadlet-nix2.nixosModules.default
      inputs.sops-nix.nixosModules.sops
      ../modules/services/invoiceninja.nix
      ../modules/services/onepassword-systemd-credentials.nix
      {
        options = {
          modules.services.caddy.routes = lib.mkOption {
            type = lib.types.attrs;
            default = { };
          };
          modules.zfs.datasets.properties = lib.mkOption {
            type = lib.types.attrs;
            default = { };
          };
        };
      }
      {
        nixpkgs.pkgs = pkgs;
        system.stateVersion = "26.05";
        boot.loader.grub.devices = [ "nodev" ];
        fileSystems."/" = {
          device = "none";
          fsType = "tmpfs";
        };
        modules.services.onepassword-systemd-credentials = {
          enable = true;
          connectHost = "http://127.0.0.1:8080";
          bootstrapTokenFile = "/run/test-connect-token";
        };
        modules.services.invoiceninja = {
          enable = true;
          domain = "invoice.example.test";
          ports.http = 8080;
          subnet.hostAddr = "10.89.2.0/24";
          user = {
            name = "invoiceninja2";
            uid = 3015;
          };
          group = {
            name = "invoiceninja2";
            gid = 3015;
          };
        };
      }
    ];
  };
  cfg = evaluated.config;
  setup = cfg.systemd.services.invoiceninja-env-setup;
  containers = cfg.virtualisation.quadlet.containers;
in
assert containers.invoiceninja-app.autoStart == false;
assert containers.invoiceninja-scheduler.autoStart == false;
assert containers.invoiceninja-worker.autoStart == false;
assert
  !(builtins.elem "invoiceninja-env-setup.service" (
    containers.invoiceninja-app.unitConfig.Requires or [ ]
  ));
assert setup.wantedBy == [ "multi-user.target" ];
assert builtins.elem "user@3015.service" setup.after;
assert lib.hasInfix "--machine=invoiceninja2@.host --user restart" setup.script;
pkgs.runCommand "invoiceninja-test" { } ''
  touch "$out"
''
