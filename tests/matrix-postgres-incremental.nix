{
  inputs,
  pkgs,
}:
let
  lib = inputs.nixpkgs.lib;
  secretFile = pkgs.writeText "matrix-postgres-incremental-test-secrets.yaml" "{}\n";
  testOptions =
    { lib, ... }:
    {
      options = {
        repo.secrets.home-ops.subnets.pg-matrix-synapse = lib.mkOption {
          type = lib.types.attrs;
        };
        site.net.mgmt.hosts4 = lib.mkOption {
          type = lib.types.attrsOf (lib.types.listOf lib.types.str);
        };
        modules.services.matrix-synapse = lib.mkOption {
          type = lib.types.attrs;
        };
      };
    };
  hostConfig =
    (lib.nixosSystem {
      specialArgs = { inherit inputs; };
      modules = [
        inputs.sops-nix.nixosModules.sops
        ../modules/services/onepassword-systemd-credentials.nix
        ../modules/services/matrix-synapse-postgres.nix
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
          repo.secrets.home-ops.subnets.pg-matrix-synapse = {
            hostAddr = "192.0.2.1";
            containerAddr = "192.0.2.2";
          };
          modules.services.matrix-synapse = {
            enable = true;
            dataDir = "/var/lib/matrix-synapse";
            user = {
              uid = 1000;
              name = "matrix-synapse";
            };
            group = {
              gid = 1000;
              name = "matrix-synapse";
            };
            bridgesGroup = {
              gid = 1001;
              name = "matrix-bridges";
            };
            bridges.discord = {
              enable = true;
              user = {
                uid = 1001;
                name = "mautrix-discord";
              };
            };
          };
        }
      ];
    }).config;
  containerConfig = hostConfig.containers.pg-matrix.config;
  provider = hostConfig.modules.services.onepassword-systemd-credentials;
  roleService = hostConfig.systemd.services.databasus-pg-matrix-role;
  proxyService = hostConfig.systemd.services.databasus-pg-matrix-proxy;
  proxySocket = hostConfig.systemd.sockets.databasus-pg-matrix-proxy;
  synapseService = hostConfig.systemd.services.matrix-synapse;
  discordService = hostConfig.systemd.services.mautrix-discord;
  authentication = containerConfig.services.postgresql.authentication;
in
assert
  hostConfig.containers.pg-matrix.bindMounts.pg-matrix-synapse.hostPath
  == "/var/lib/matrix-synapse/pg-18";
assert containerConfig.services.postgresql.package.version == pkgs.postgresql_18.version;
assert containerConfig.services.postgresql.settings.wal_level == "replica";
assert containerConfig.services.postgresql.settings.summarize_wal == "on";
assert containerConfig.services.postgresql.settings.max_wal_senders > 0;
assert containerConfig.services.postgresql.settings.max_replication_slots > 0;
assert lib.hasInfix "local replication databasus_pg_matrix scram-sha-256" authentication;
assert lib.hasInfix "local postgres databasus_pg_matrix scram-sha-256" authentication;
assert lib.hasInfix "local all databasus_pg_matrix reject" authentication;
assert
  provider.consumers.databasus-pg-matrix-role == {
    POSTGRES_PASSWORD = "op://home-ops-prod/databasus-pg-matrix/password";
  };
assert roleService.serviceConfig.User == "postgres";
assert builtins.elem "container@pg-matrix.service" roleService.requires;
assert builtins.elem "onepassword-credential-provider.socket" roleService.requires;
assert builtins.elem "onepassword-credential-provider.socket" roleService.after;
assert
  roleService.serviceConfig.LoadCredential == [
    "POSTGRES_PASSWORD:/run/onepassword-credential-provider.sock"
  ];
assert builtins.length roleService.serviceConfig.ExecStartPre == 2;
assert builtins.elem "${pkgs.coreutils}/bin/test -S /run/postgresql-matrix-synapse/.s.PGSQL.5432"
  roleService.serviceConfig.ExecStartPre;
assert builtins.elem "${pkgs.coreutils}/bin/test -s %d/POSTGRES_PASSWORD"
  roleService.serviceConfig.ExecStartPre;
assert lib.hasInfix "CREATE ROLE databasus_pg_matrix" roleService.script;
assert lib.hasInfix "LOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE REPLICATION NOBYPASSRLS"
  roleService.script;
assert builtins.elem "container@pg-matrix.service" synapseService.requires;
assert builtins.elem "container@pg-matrix.service" synapseService.after;
assert builtins.elem "container@pg-matrix.service" discordService.requires;
assert builtins.elem "container@pg-matrix.service" discordService.after;
assert proxySocket.socketConfig.ListenStream == "192.0.2.14:5433";
assert builtins.elem "container@pg-matrix.service" proxyService.requires;
assert lib.hasInfix "systemd-socket-proxyd /run/postgresql-matrix-synapse/.s.PGSQL.5432"
  proxyService.serviceConfig.ExecStart;
assert !(builtins.elem 5433 hostConfig.networking.firewall.allowedTCPPorts);
assert lib.hasInfix ''iifname "mgmt"'' hostConfig.networking.firewall.extraInputRules;
assert lib.hasInfix "ip saddr 192.0.2.3/32" hostConfig.networking.firewall.extraInputRules;
assert lib.hasInfix "ip daddr 192.0.2.14" hostConfig.networking.firewall.extraInputRules;
assert lib.hasInfix "tcp dport 5433 accept" hostConfig.networking.firewall.extraInputRules;
pkgs.runCommand "matrix-postgres-incremental" { } "touch $out"
