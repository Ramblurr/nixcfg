{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.services.matrix-synapse;

  mounts = {
    pg-matrix-synapse = {
      mountPoint = "/db";
      hostPath = "${cfg.dataDir}/pg";
      isReadOnly = false;
    };
    host-socket = {
      mountPoint = "/run/postgresql";
      hostPath = "/run/postgresql-matrix-synapse";
      isReadOnly = false;
    };
  };
in
{
  # On my host I run a "fat" postgresql database for all my apps
  # but synapse's postgres instance outstrips them all in resource usage, so I want to keep it separate
  # So I use a declarative nixos container for postgres
  config = lib.mkIf cfg.enable {
    programs.extra-container.enable = true;
    networking.nat.enable = true;
    networking.nat.internalInterfaces = [ "ve-pg-matrix" ];
    networking.nat.externalInterface = "brmgmt9";
    containers.pg-matrix = {
      autoStart = true;
      privateNetwork = true;
      hostAddress = config.repo.secrets.home-ops.subnets.pg-matrix-synapse.hostAddr;
      localAddress = config.repo.secrets.home-ops.subnets.pg-matrix-synapse.containerAddr;
      bindMounts = {
        inherit (mounts) pg-matrix-synapse host-socket;
        "/etc/resolv.conf" = {
          hostPath = "/etc/resolv-external.conf";
          isReadOnly = true;
        };
      };

      config =
        {
          pkgs,
          config,
          lib,
          ...
        }:
        {
          # inside here is a separate nixos configuration for the container
          networking.firewall.enable = false;
          documentation.nixos.enable = false;
          users.users.matrix-synapse = {
            uid = cfg.user.uid;
            isSystemUser = true;
            group = cfg.group.name;
            createHome = false;
          };

          users.users.${cfg.bridges.discord.user.name} = lib.mkIf cfg.bridges.discord.enable {
            uid = cfg.bridges.discord.user.uid;
            isSystemUser = true;
            group = cfg.bridgesGroup.name;
          };

          users.groups.matrix-synapse = {
            gid = lib.mkForce cfg.group.gid;
          };

          users.groups.${cfg.bridgesGroup.name} = {
            gid = lib.mkForce cfg.bridgesGroup.gid;
          };

          environment.systemPackages = with pkgs; [
            pgbackrest
            kitty.terminfo
          ];

          services.postgresql = {
            enable = true;
            package = pkgs.postgresql_15;
            extraPlugins = with config.services.postgresql.package.pkgs; [ pgaudit ];
            dataDir = mounts.pg-matrix-synapse.mountPoint;
            port = 5432;
            enableTCPIP = false;
            initialScript = pkgs.writeText "synapse-init.sql" ''
              CREATE ROLE "matrix-synapse";
              CREATE DATABASE "matrix-synapse" WITH OWNER "matrix-synapse"
                TEMPLATE template0
                LC_COLLATE = "C"
                LC_CTYPE = "C";
              GRANT ALL PRIVILEGES ON DATABASE "matrix-synapse" TO "matrix-synapse";
              ALTER ROLE "matrix-synapse" WITH LOGIN;

              CREATE ROLE "mautrix-discord";
              CREATE DATABASE "mautrix-discord" WITH OWNER "mautrix-discord" TEMPLATE template0;
              GRANT ALL PRIVILEGES ON DATABASE "mautrix-discord" TO "mautrix-discord";
              ALTER ROLE "mautrix-discord" WITH LOGIN;
            '';
            settings = {
              unix_socket_directories = "/tmp,${mounts.host-socket.mountPoint}";

              #archive_mode = "on";
              #archive_command = "${pkgs.pgbackrest}/bin/pgbackrest --stanza=${stanza} archive-push %p";
              #max_wal_senders = 3;
              #wal_level = "replica";
            };
          };
        };
    };
  };
}
