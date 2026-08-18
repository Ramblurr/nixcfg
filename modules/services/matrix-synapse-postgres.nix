{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.services.matrix-synapse;
  backupRole = "databasus_pg_matrix";
  maliMgmtAddress = builtins.head config.site.net.mgmt.hosts4.mali;
  deweyMgmtAddress = builtins.head config.site.net.mgmt.hosts4.${config.networking.hostName};

  mounts = {
    pg-matrix-synapse = {
      mountPoint = "/db";
      hostPath = "${cfg.dataDir}/pg-18";
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
    networking.nat.externalInterface = "mgmt";

    assertions = [
      {
        assertion = config.modules.services.onepassword-systemd-credentials.enable;
        message = "pg-matrix Databasus credentials require the systemd credential provider.";
      }
      {
        assertion = !(builtins.elem 5433 config.networking.firewall.allowedTCPPorts);
        message = "The pg-matrix Databasus proxy must not use a globally allowed TCP port.";
      }
    ];

    modules.services.onepassword-systemd-credentials.consumers.databasus-pg-matrix-role = {
      POSTGRES_PASSWORD = "op://home-ops-prod/databasus-pg-matrix/password";
    };

    networking.firewall.extraInputRules = ''
      iifname "mgmt" ip saddr ${maliMgmtAddress}/32 ip daddr ${deweyMgmtAddress} tcp dport 5433 accept comment "Databasus pg-matrix physical backup"
    '';

    systemd.services.matrix-synapse = {
      requires = [ "container@pg-matrix.service" ];
      after = [ "container@pg-matrix.service" ];
    };

    systemd.services.mautrix-discord = lib.mkIf cfg.bridges.discord.enable {
      requires = [ "container@pg-matrix.service" ];
      after = [ "container@pg-matrix.service" ];
    };

    systemd.sockets.databasus-pg-matrix-proxy = {
      description = "Databasus proxy for the pg-matrix PostgreSQL socket";
      wantedBy = [ "sockets.target" ];
      socketConfig = {
        ListenStream = "${deweyMgmtAddress}:5433";
        FreeBind = true;
        NoDelay = true;
      };
    };

    systemd.services.databasus-pg-matrix-proxy = {
      description = "Proxy Databasus connections to the pg-matrix PostgreSQL socket";
      requires = [
        "container@pg-matrix.service"
        "databasus-pg-matrix-proxy.socket"
      ];
      after = [
        "container@pg-matrix.service"
        "databasus-pg-matrix-proxy.socket"
      ];
      serviceConfig = {
        ExecStart = "${pkgs.systemd}/lib/systemd/systemd-socket-proxyd ${mounts.host-socket.hostPath}/.s.PGSQL.5432";
        DynamicUser = true;
        PrivateTmp = true;
        PrivateDevices = true;
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ProtectControlGroups = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectKernelLogs = true;
        RestrictAddressFamilies = [
          "AF_UNIX"
          "AF_INET"
          "AF_INET6"
        ];
      };
    };

    systemd.services.databasus-pg-matrix-role = {
      description = "Provision the Databasus replication role for pg-matrix";
      wantedBy = [ "multi-user.target" ];
      requires = [ "container@pg-matrix.service" ];
      after = [ "container@pg-matrix.service" ];
      script = ''
        ${pkgs.postgresql_18}/bin/psql \
          --no-psqlrc \
          --quiet \
          --set ON_ERROR_STOP=1 \
          --host ${mounts.host-socket.hostPath} \
          --dbname postgres <<'SQL'
        \set password `cat "$CREDENTIALS_DIRECTORY/POSTGRES_PASSWORD"`
        SELECT 'CREATE ROLE ${backupRole} LOGIN REPLICATION'
        WHERE NOT EXISTS (SELECT FROM pg_roles WHERE rolname = '${backupRole}') \gexec
        ALTER ROLE ${backupRole}
          WITH LOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE REPLICATION NOBYPASSRLS;
        SELECT format('ALTER ROLE %I PASSWORD %L', '${backupRole}', :'password') \gexec
        REVOKE ALL PRIVILEGES ON DATABASE postgres FROM ${backupRole};
        GRANT CONNECT ON DATABASE postgres TO ${backupRole};
        SQL
      '';
      serviceConfig = {
        Type = "oneshot";
        User = "postgres";
        Group = "postgres";
        RemainAfterExit = true;
        ExecStartPre = "${pkgs.coreutils}/bin/test -S ${mounts.host-socket.hostPath}/.s.PGSQL.5432";
      };
    };
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
          system.stateVersion = "24.11";
          networking.firewall.enable = false;
          documentation.nixos.enable = false;
          users.users.matrix-synapse = {
            inherit (cfg.user) uid;
            isSystemUser = true;
            group = cfg.group.name;
            createHome = false;
          };

          users.users.${cfg.bridges.discord.user.name} = lib.mkIf cfg.bridges.discord.enable {
            inherit (cfg.bridges.discord.user) uid;
            isSystemUser = true;
            group = cfg.bridgesGroup.name;
          };

          users.groups.matrix-synapse = {
            gid = lib.mkForce cfg.group.gid;
          };

          users.groups.${cfg.bridgesGroup.name} = {
            gid = lib.mkForce cfg.bridgesGroup.gid;
          };

          environment.systemPackages = map (x: x.terminfo) (
            with pkgs.pkgsBuildBuild;
            [
              ghostty
              kitty
              tmux
              wezterm
            ]
          );

          services.postgresql = {
            enable = true;
            package = pkgs.postgresql_18;
            extensions = with config.services.postgresql.package.pkgs; [ pgaudit ];
            dataDir = mounts.pg-matrix-synapse.mountPoint;
            enableTCPIP = false;
            authentication = ''
              local replication ${backupRole} scram-sha-256
              local postgres ${backupRole} scram-sha-256
              local all ${backupRole} reject
            '';
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
              wal_level = "replica";
              summarize_wal = "on";
              max_wal_senders = 10;
              max_replication_slots = 10;
              port = 5432;
            };
          };
        };
    };
  };
}
