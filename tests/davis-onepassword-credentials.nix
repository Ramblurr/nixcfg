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
  backupCredentialDirectory = "/run/credentials/databasus-davis-role.service";
  backupPasswordPath = "${backupCredentialDirectory}/POSTGRES_PASSWORD";
  expectedCredentials = {
    ADMIN_PASSWORD = "op://home-ops-prod/davis/ADMIN_PASSWORD";
    APP_SECRET = "op://home-ops-prod/davis/APP_SECRET";
  };
  expectedBackupCredentials = {
    POSTGRES_PASSWORD = "op://home-ops-prod/databasus-davis/password";
  };
  testOptions =
    { lib, ... }:
    {
      options = {
        repo.secrets = lib.mkOption {
          type = lib.types.attrs;
          default = { };
        };
        site.net = lib.mkOption {
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
        ../modules/services/postgresql.nix
        ../modules/site/gatus.nix
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
          site.net.mgmt.hosts4 = {
            dewey = [ "192.0.2.14" ];
            mali = [ "192.0.2.3" ];
            onepassword-connect = [ "192.0.2.22" ];
          };
          networking.firewall.allowedTCPPorts = [ 3306 ];
          repo.secrets.home-ops.mail = {
            dsn = "smtp://mail.example.test";
            notificationsFromAddress = "notifications@example.test";
            imapAuthUrlNew = "imaps://mail.example.test";
          };
          modules.services.postgresql.enable = true;
          modules.services.davis = {
            enable = true;
            domain = "dav.example.test";
          };
        }
      ];
    }).config;
  provider = cfg.modules.services.onepassword-systemd-credentials;
  creds = provider.creds.davis-env-setup;
  service = cfg.systemd.services.davis-env-setup;
  execStart = service.serviceConfig.ExecStart;
  backupCreds = provider.creds.databasus-davis-role;
  backupService = cfg.systemd.services.databasus-davis-role;
  backupScript = backupService.script;
in
assert provider.enable;
assert provider.consumers.davis-env-setup == expectedCredentials;
assert
  creds == {
    ADMIN_PASSWORD = adminPasswordPath;
    APP_SECRET = appSecretPath;
  };
assert cfg.services.davis.appSecretFile == creds.APP_SECRET;
assert cfg.services.davis.adminPasswordFile == creds.ADMIN_PASSWORD;
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
assert provider.consumers.databasus-davis-role == expectedBackupCredentials;
assert backupCreds.POSTGRES_PASSWORD == backupPasswordPath;
assert builtins.elem "postgresql.service" backupService.requires;
assert builtins.elem "onepassword-credential-provider.socket" backupService.requires;
assert builtins.elem "postgresql.service" backupService.after;
assert builtins.elem "onepassword-credential-provider.socket" backupService.after;
assert
  backupService.serviceConfig.LoadCredential == [
    "POSTGRES_PASSWORD:/run/onepassword-credential-provider.sock"
  ];
assert
  backupService.serviceConfig.ExecStartPre == [
    "${pkgs.coreutils}/bin/test -s %d/POSTGRES_PASSWORD"
  ];
assert backupService.serviceConfig.User == "postgres";
assert lib.hasInfix "CREATE ROLE databasus_davis" backupScript;
assert lib.hasInfix "NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION NOBYPASSRLS" backupScript;
assert lib.hasInfix "REVOKE TEMP ON DATABASE davis FROM PUBLIC" backupScript;
assert lib.hasInfix "GRANT SELECT ON ALL TABLES IN SCHEMA public TO databasus_davis" backupScript;
assert lib.hasInfix "ALTER DEFAULT PRIVILEGES FOR ROLE davis IN SCHEMA public" backupScript;
assert
  cfg.modules.services.postgresql.extraAuthentication == [
    "host davis databasus_davis 192.0.2.3/32 scram-sha-256"
    "host all databasus_davis 192.0.2.3/32 reject"
  ];
assert !(builtins.elem 5432 cfg.networking.firewall.allowedTCPPorts);
assert builtins.elem 3306 cfg.networking.firewall.allowedTCPPorts;
assert lib.hasInfix ''iifname "mgmt"'' cfg.networking.firewall.extraInputRules;
assert lib.hasInfix "ip saddr 192.0.2.3/32" cfg.networking.firewall.extraInputRules;
assert lib.hasInfix "ip daddr 192.0.2.14" cfg.networking.firewall.extraInputRules;
assert lib.hasInfix "tcp dport 5432 accept" cfg.networking.firewall.extraInputRules;
assert !(builtins.elem "sops-install-secrets.service" service.requires);
assert !(builtins.elem "sops-install-secrets.service" service.after);
assert !(builtins.hasAttr "davis/APP_SECRET" cfg.sops.secrets);
assert !(builtins.hasAttr "davis/ADMIN_PASSWORD" cfg.sops.secrets);
pkgs.runCommand "davis-onepassword-credentials" { } "touch $out"
