{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.services.hindsight;
  dbEnvironmentFile = config.sops.templates."hindsight-db.env".path;
  appEnvironmentFile = config.sops.templates."hindsight-app.env".path;
  postgresInit = pkgs.writeText "hindsight-postgres-init.sql" ''
    CREATE EXTENSION IF NOT EXISTS vector;
  '';
in
{
  options.modules.services.hindsight = {
    enable = lib.mkEnableOption "Hindsight agent memory";

    domain = lib.mkOption {
      type = lib.types.str;
      example = "hindsight.example.com";
      description = "Internal domain used to expose Hindsight.";
    };

    acmeHost = lib.mkOption {
      type = lib.types.str;
      example = "example.com";
      description = "Existing ACME certificate host used by the internal ingress.";
    };

    dataDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/hindsight";
      description = "Persistent home and rootless Podman storage for Hindsight.";
    };

    user = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "hindsight";
        description = "User that owns and runs the rootless Hindsight Quadlets.";
      };

      uid = lib.mkOption {
        type = lib.types.int;
        default = 3020;
        description = "UID of the rootless Hindsight service user.";
      };
    };

    group = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "hindsight";
        description = "Primary group of the Hindsight service user.";
      };

      gid = lib.mkOption {
        type = lib.types.int;
        default = 3020;
        description = "GID of the Hindsight service group.";
      };
    };

    image = lib.mkOption {
      type = lib.types.str;
      # renovate: docker-image
      default = "ghcr.io/vectorize-io/hindsight:0.8.4";
      description = "Hindsight standalone container image.";
    };

    postgresImage = lib.mkOption {
      type = lib.types.str;
      # renovate: docker-image
      default = "docker.io/pgvector/pgvector:pg18";
      description = "PostgreSQL container image with pgvector installed.";
    };

    ports = {
      api = lib.mkOption {
        type = lib.types.port;
        default = 8888;
        description = "Loopback port for the Hindsight API.";
      };

      controlPlane = lib.mkOption {
        type = lib.types.port;
        default = 9999;
        description = "Loopback port for the Hindsight control plane.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.ports.api != cfg.ports.controlPlane;
        message = "Hindsight API and control-plane ports must be different.";
      }
    ];

    users.users.${cfg.user.name} = {
      inherit (cfg.user) name;
      uid = lib.mkForce cfg.user.uid;
      isSystemUser = true;
      group = cfg.group.name;
      home = cfg.dataDir;
      createHome = false;
      linger = true;
      autoSubUidGidRange = true;
    };

    users.groups.${cfg.group.name} = {
      inherit (cfg.group) name;
      gid = lib.mkForce cfg.group.gid;
    };

    environment.persistence."/persist".directories = [
      {
        directory = cfg.dataDir;
        user = cfg.user.name;
        group = cfg.group.name;
        mode = "0750";
      }
    ];

    sops.secrets = {
      "hindsight/openai-api-key" = { };
      "hindsight/postgres-password" = { };
      "hindsight/api-key" = { };
      "hindsight/control-plane-access-key" = { };
    };

    sops.templates = {
      "hindsight-db.env" = {
        owner = cfg.user.name;
        group = cfg.group.name;
        mode = "0400";
        content = ''
          POSTGRES_PASSWORD=${config.sops.placeholder."hindsight/postgres-password"}
        '';
      };

      "hindsight-app.env" = {
        owner = cfg.user.name;
        group = cfg.group.name;
        mode = "0400";
        content = ''
          HINDSIGHT_API_DATABASE_URL=postgresql://hindsight:${
            config.sops.placeholder."hindsight/postgres-password"
          }@hindsight-db:5432/hindsight
          HINDSIGHT_API_LLM_API_KEY=${config.sops.placeholder."hindsight/openai-api-key"}
          HINDSIGHT_API_TENANT_API_KEY=${config.sops.placeholder."hindsight/api-key"}
          HINDSIGHT_CP_DATAPLANE_API_KEY=${config.sops.placeholder."hindsight/api-key"}
          HINDSIGHT_CP_ACCESS_KEY=${config.sops.placeholder."hindsight/control-plane-access-key"}
        '';
      };
    };

    virtualisation.quadlet = {
      enable = true;

      networks.hindsight = {
        uid = cfg.user.uid;
        autoStart = true;
        networkConfig.NetworkName = "hindsight";
      };

      volumes.hindsight-db-data = {
        uid = cfg.user.uid;
        autoStart = true;
        volumeConfig.VolumeName = "hindsight-db-data";
      };

      containers = {
        hindsight-db = {
          uid = cfg.user.uid;
          autoStart = true;
          unitConfig = {
            After = [ "network-online.target" ];
            Wants = [ "network-online.target" ];
          };
          serviceConfig = {
            ExecStartPre = [ "${pkgs.coreutils}/bin/test -r ${dbEnvironmentFile}" ];
            Restart = "on-failure";
            RestartSec = "5s";
            TimeoutStopSec = "120s";
          };
          containerConfig = {
            Image = cfg.postgresImage;
            ContainerName = "hindsight-db";
            Network = "hindsight.network";
            EnvironmentFile = [ dbEnvironmentFile ];
            Environment = [
              "POSTGRES_USER=hindsight"
              "POSTGRES_DB=hindsight"
            ];
            Volume = [
              "hindsight-db-data.volume:/var/lib/postgresql/18/docker"
              "${postgresInit}:/docker-entrypoint-initdb.d/10-hindsight.sql:ro"
            ];
            HealthCmd = "pg_isready -U hindsight -d hindsight";
            HealthInterval = "10s";
            HealthTimeout = "5s";
            HealthRetries = 10;
            HealthStartPeriod = "30s";
            HealthOnFailure = "kill";
            Notify = "healthy";
          };
        };

        hindsight = {
          uid = cfg.user.uid;
          autoStart = true;
          unitConfig = {
            Requires = [ "hindsight-db.service" ];
            After = [
              "hindsight-db.service"
              "network-online.target"
            ];
            Wants = [ "network-online.target" ];
          };
          serviceConfig = {
            ExecStartPre = [ "${pkgs.coreutils}/bin/test -r ${appEnvironmentFile}" ];
            Restart = "on-failure";
            RestartSec = "5s";
            TimeoutStartSec = "900s";
            TimeoutStopSec = "120s";
          };
          containerConfig = {
            Image = cfg.image;
            ContainerName = "hindsight";
            Network = "hindsight.network";
            EnvironmentFile = [ appEnvironmentFile ];
            Environment = [
              "HINDSIGHT_API_VECTOR_EXTENSION=pgvector"
              "HINDSIGHT_API_LLM_PROVIDER=openai"
              "HINDSIGHT_API_LLM_MODEL=gpt-5-mini"
              "HINDSIGHT_API_WORKER_ID=${config.networking.hostName}"
              "HINDSIGHT_API_HOST=0.0.0.0"
              "HINDSIGHT_API_PORT=8888"
              "HINDSIGHT_CP_DATAPLANE_API_URL=http://127.0.0.1:8888"
              "HINDSIGHT_API_TENANT_EXTENSION=hindsight_api.extensions.builtin.tenant:ApiKeyTenantExtension"
            ];
            PublishPort = [
              "127.0.0.1:${toString cfg.ports.api}:8888"
              "127.0.0.1:${toString cfg.ports.controlPlane}:9999"
            ];
            HealthCmd = "curl --fail http://127.0.0.1:8888/health";
            HealthInterval = "30s";
            HealthTimeout = "10s";
            HealthRetries = 5;
            HealthStartPeriod = "120s";
            HealthOnFailure = "kill";
          };
        };
      };
    };

    modules.services.ingress.virtualHosts.${cfg.domain} = {
      inherit (cfg) acmeHost;
      upstream = "http://127.0.0.1:${toString cfg.ports.controlPlane}";
      forwardAuth = false;
    };

    services.nginx.virtualHosts.${cfg.domain}.locations."/v1/" = {
      proxyPass = "http://127.0.0.1:${toString cfg.ports.api}";
      recommendedProxySettings = true;
    };
  };
}
