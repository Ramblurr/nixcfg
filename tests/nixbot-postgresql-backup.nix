{
  inputs,
  pkgs,
}:
let
  lib = inputs.nixpkgs.lib;
  secretFile = pkgs.writeText "nixbot-postgresql-backup-test-secrets.yaml" "{}\n";
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
        modules.zfs.datasets.properties = lib.mkOption {
          type = lib.types.attrsOf (lib.types.attrsOf lib.types.str);
          default = { };
        };
      };
    };
  cfg =
    (lib.nixosSystem {
      specialArgs = {
        inherit inputs;
        self = { inherit inputs; };
      };
      modules = [
        inputs.sops-nix.nixosModules.sops
        inputs.nixbot.nixosModules.nixbot
        ../modules/services/onepassword-systemd-credentials.nix
        ../hosts/debord/nixbot.nix
        testOptions
        {
          nixpkgs.pkgs = pkgs;
          networking.hostName = "debord";
          system.stateVersion = "26.05";
          boot.loader.grub.devices = [ "nodev" ];
          fileSystems."/" = {
            device = "none";
            fsType = "tmpfs";
          };
          sops.defaultSopsFile = secretFile;
          sops.age.keyFile = "/tmp/age-key.txt";
          site.net.mgmt.hosts4 = {
            debord = [ "192.0.2.21" ];
            mali = [ "192.0.2.3" ];
            onepassword-connect = [ "192.0.2.22" ];
          };
          repo.secrets = {
            global = {
              domain.work = "example.test";
              localAtticSubstituter = "https://attic.example.test/home";
            };
            home-ops.ports.nixbot = 10000;
            local.nixbot = {
              appId = 1;
              oauthId = "test";
            };
          };
        }
      ];
    }).config;
  provider = cfg.modules.services.onepassword-systemd-credentials;
  service = cfg.systemd.services.databasus-nixbot-role;
  script = service.script;
in
assert
  provider.consumers.databasus-nixbot-role == {
    POSTGRES_PASSWORD = "op://home-ops-prod/databasus-nixbot/password";
  };
assert
  service.serviceConfig.LoadCredential == [
    "POSTGRES_PASSWORD:/run/onepassword-credential-provider.sock"
  ];
assert builtins.elem "postgresql.service" service.requires;
assert builtins.elem "onepassword-credential-provider.socket" service.requires;
assert builtins.elem "postgresql.service" service.after;
assert builtins.elem "onepassword-credential-provider.socket" service.after;
assert service.serviceConfig.User == "postgres";
assert lib.hasInfix "CREATE ROLE databasus_nixbot LOGIN" script;
assert lib.hasInfix "NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION NOBYPASSRLS" script;
assert lib.hasInfix "GRANT SELECT ON ALL TABLES IN SCHEMA public TO databasus_nixbot" script;
assert lib.hasInfix "GRANT SELECT ON ALL SEQUENCES IN SCHEMA public TO databasus_nixbot" script;
assert lib.hasInfix "ALTER DEFAULT PRIVILEGES FOR ROLE nixbot IN SCHEMA public" script;
assert cfg.services.postgresql.enableTCPIP;
assert cfg.services.postgresql.settings.listen_addresses == "192.0.2.21";
assert lib.hasInfix "host nixbot databasus_nixbot 192.0.2.3/32 scram-sha-256"
  cfg.services.postgresql.authentication;
assert lib.hasInfix "host all databasus_nixbot 192.0.2.3/32 reject"
  cfg.services.postgresql.authentication;
assert !(builtins.elem 5432 cfg.networking.firewall.allowedTCPPorts);
assert lib.hasInfix ''iifname "mgmt"'' cfg.networking.firewall.extraInputRules;
assert lib.hasInfix "ip saddr 192.0.2.3/32" cfg.networking.firewall.extraInputRules;
assert lib.hasInfix "ip daddr 192.0.2.21" cfg.networking.firewall.extraInputRules;
assert lib.hasInfix "tcp dport 5432 accept" cfg.networking.firewall.extraInputRules;
assert lib.hasInfix ''comment "Databasus Nixbot logical backup"''
  cfg.networking.firewall.extraInputRules;
pkgs.runCommand "nixbot-postgresql-backup" { } "touch $out"
