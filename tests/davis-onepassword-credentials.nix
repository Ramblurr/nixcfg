{
  inputs,
  pkgs,
}:
let
  lib = inputs.nixpkgs.lib;
  secretFile = pkgs.writeText "davis-onepassword-test-secrets.yaml" "{}\n";
  credentialDirectory = "/run/credentials/davis-env-setup.service";
  appSecretPath = "${credentialDirectory}/APP_SECRET";
  adminPasswordPath = "${credentialDirectory}/ADMIN_PASSWORD";
  expectedCredentials = {
    ADMIN_PASSWORD = "op://home-ops-prod/davis/ADMIN_PASSWORD";
    APP_SECRET = "op://home-ops-prod/davis/APP_SECRET";
  };
  testOptions =
    { lib, ... }:
    {
      options = {
        repo.secrets = lib.mkOption {
          type = lib.types.attrs;
          default = { };
        };
        site = lib.mkOption {
          type = lib.types.attrs;
          default = { };
        };
        modules.services.caddy.routes = lib.mkOption {
          type = lib.types.attrs;
          default = { };
        };
        modules.zfs.datasets.properties = lib.mkOption {
          type = lib.types.attrsOf (lib.types.attrsOf lib.types.str);
          default = { };
        };
      };
    };
  cfg =
    (lib.nixosSystem {
      specialArgs = { inherit inputs; };
      modules = [
        inputs.sops-nix.nixosModules.sops
        ../modules/services/onepassword-systemd-credentials.nix
        ../modules/services/davis.nix
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
          sops.defaultSopsFile = secretFile;
          sops.age.keyFile = "/tmp/age-key.txt";
          site.net.mgmt.hosts4.onepassword-connect = [ "192.0.2.22" ];
          repo.secrets.home-ops.mail = {
            dsn = "smtp://mail.example.test";
            notificationsFromAddress = "notifications@example.test";
            imapAuthUrlNew = "imaps://mail.example.test";
          };
          modules.services.davis = {
            enable = true;
            domain = "dav.example.test";
          };
        }
      ];
    }).config;
  service = cfg.systemd.services.davis-env-setup;
  execStart = service.serviceConfig.ExecStart;
in
assert cfg.modules.services.onepassword-systemd-credentials.enable;
assert
  cfg.modules.services.onepassword-systemd-credentials.consumers.davis-env-setup
  == expectedCredentials;
assert cfg.services.davis.appSecretFile == appSecretPath;
assert cfg.services.davis.adminPasswordFile == adminPasswordPath;
assert service.requires == [ "onepassword-credential-provider.socket" ];
assert service.after == [ "onepassword-credential-provider.socket" ];
assert
  service.serviceConfig.LoadCredential == [
    "ADMIN_PASSWORD:/run/onepassword-credential-provider.sock"
    "APP_SECRET:/run/onepassword-credential-provider.sock"
  ];
assert
  service.serviceConfig.ExecStartPre == [
    "${pkgs.coreutils}/bin/test -s %d/ADMIN_PASSWORD"
    "${pkgs.coreutils}/bin/test -s %d/APP_SECRET"
  ];
assert lib.any (lib.hasInfix adminPasswordPath) execStart;
assert lib.any (lib.hasInfix appSecretPath) execStart;
assert !(builtins.elem "sops-install-secrets.service" service.requires);
assert !(builtins.elem "sops-install-secrets.service" service.after);
assert builtins.hasAttr "davis/APP_SECRET" cfg.sops.secrets;
assert builtins.hasAttr "davis/ADMIN_PASSWORD" cfg.sops.secrets;
assert cfg.sops.secrets."davis/APP_SECRET".path != appSecretPath;
assert cfg.sops.secrets."davis/ADMIN_PASSWORD".path != adminPasswordPath;
pkgs.runCommand "davis-onepassword-credentials" { } "touch $out"
