{
  config,
  lib,
  pkgs,
  ...
}:

let
  cerebrasBaseUrl = "https://api.cerebras.ai/v1";
  llmProfiles = {
    openai-gpt-5-mini = {
      provider = "openai";
      model = "gpt-5-mini";
      apiKeySecret = "hindsight/openai-api-key";
      codexOAuth = false;
      baseUrl = null;
      maxConcurrent = null;
    };

    "openai-gpt-4.1-nano" = {
      provider = "openai";
      model = "gpt-4.1-nano";
      apiKeySecret = "hindsight/openai-api-key";
      codexOAuth = false;
      baseUrl = null;
      maxConcurrent = null;
    };

    "openai-codex-gpt-5.4-mini" = {
      provider = "openai-codex";
      model = "gpt-5.4-mini";
      apiKeySecret = null;
      codexOAuth = true;
      baseUrl = null;
      maxConcurrent = null;
    };

    cerebras-gpt-oss-120b = {
      provider = "openai";
      model = "gpt-oss-120b";
      apiKeySecret = "hindsight/cerebras-api-key";
      codexOAuth = false;
      baseUrl = cerebrasBaseUrl;
      maxConcurrent = 2;
    };

    cerebras-gemma-4-31b = {
      provider = "openai";
      model = "gemma-4-31b";
      apiKeySecret = "hindsight/cerebras-api-key";
      codexOAuth = false;
      baseUrl = cerebrasBaseUrl;
      maxConcurrent = 2;
    };

    "gemini-3.1-flash-lite" = {
      provider = "gemini";
      model = "gemini-3.1-flash-lite";
      apiKeySecret = "hindsight/gemini-api-key";
      codexOAuth = false;
      baseUrl = null;
      maxConcurrent = null;
    };
  };

  embeddingsProfiles = {
    openai-small = {
      provider = "openai";
      model = "text-embedding-3-small";
      modelEnvironment = "HINDSIGHT_API_EMBEDDINGS_OPENAI_MODEL";
      apiKeySecret = "hindsight/openai-api-key";
      codexOAuth = false;
    };

    openai-large = {
      provider = "openai";
      model = "text-embedding-3-large";
      modelEnvironment = "HINDSIGHT_API_EMBEDDINGS_OPENAI_MODEL";
      apiKeySecret = "hindsight/openai-api-key";
      codexOAuth = false;
    };

    openai-codex-small = {
      provider = "openai-codex";
      model = "text-embedding-3-small";
      modelEnvironment = "HINDSIGHT_API_EMBEDDINGS_OPENAI_MODEL";
      apiKeySecret = null;
      codexOAuth = true;
    };

    openai-codex-large = {
      provider = "openai-codex";
      model = "text-embedding-3-large";
      modelEnvironment = "HINDSIGHT_API_EMBEDDINGS_OPENAI_MODEL";
      apiKeySecret = null;
      codexOAuth = true;
    };

    "local-bge-small-en-v1.5" = {
      provider = "local";
      model = "BAAI/bge-small-en-v1.5";
      modelEnvironment = "HINDSIGHT_API_EMBEDDINGS_LOCAL_MODEL";
      apiKeySecret = null;
      codexOAuth = false;
    };
  };

  cfg = config.modules.services.hindsight;
  retainLlmProfile = llmProfiles.${cfg.llm.retain.profile};
  reflectLlmProfile = llmProfiles.${cfg.llm.reflect.profile};
  embeddingsProfile = embeddingsProfiles.${cfg.embeddings.profile};
  usesCodexOAuth =
    retainLlmProfile.codexOAuth || reflectLlmProfile.codexOAuth || embeddingsProfile.codexOAuth;
  usesCerebras =
    retainLlmProfile.baseUrl == cerebrasBaseUrl || reflectLlmProfile.baseUrl == cerebrasBaseUrl;
  dbEnvironmentFile = config.sops.templates."hindsight-db.env".path;
  appEnvironmentFile = config.sops.templates."hindsight-app.env".path;
  codexAuthDir = "${cfg.dataDir}/codex";
  codexContainerDir = "/var/lib/hindsight/codex";
  codexAuthFile = "${codexAuthDir}/auth.json";
  providerSecretEnvironment =
    lib.optionalAttrs (retainLlmProfile.apiKeySecret != null) {
      HINDSIGHT_API_LLM_API_KEY = retainLlmProfile.apiKeySecret;
      HINDSIGHT_API_RETAIN_LLM_API_KEY = retainLlmProfile.apiKeySecret;
    }
    // lib.optionalAttrs (reflectLlmProfile.apiKeySecret != null) {
      HINDSIGHT_API_REFLECT_LLM_API_KEY = reflectLlmProfile.apiKeySecret;
    }
    // lib.optionalAttrs (embeddingsProfile.apiKeySecret != null) {
      HINDSIGHT_API_EMBEDDINGS_OPENAI_API_KEY = embeddingsProfile.apiKeySecret;
    };
  requiredProviderSecrets = lib.unique (builtins.attrValues providerSecretEnvironment);
  providerEnvironment = [
    "HINDSIGHT_API_LLM_PROVIDER=${retainLlmProfile.provider}"
    "HINDSIGHT_API_LLM_MODEL=${retainLlmProfile.model}"
    "HINDSIGHT_API_RETAIN_LLM_PROVIDER=${retainLlmProfile.provider}"
    "HINDSIGHT_API_RETAIN_LLM_MODEL=${retainLlmProfile.model}"
    "HINDSIGHT_API_REFLECT_LLM_PROVIDER=${reflectLlmProfile.provider}"
    "HINDSIGHT_API_REFLECT_LLM_MODEL=${reflectLlmProfile.model}"
  ]
  ++ lib.optional (
    retainLlmProfile.baseUrl != null
  ) "HINDSIGHT_API_LLM_BASE_URL=${retainLlmProfile.baseUrl}"
  ++ lib.optional (
    retainLlmProfile.baseUrl != null
  ) "HINDSIGHT_API_RETAIN_LLM_BASE_URL=${retainLlmProfile.baseUrl}"
  ++ lib.optional (
    reflectLlmProfile.baseUrl != null
  ) "HINDSIGHT_API_REFLECT_LLM_BASE_URL=${reflectLlmProfile.baseUrl}"
  ++ lib.optional (
    retainLlmProfile.maxConcurrent != null
  ) "HINDSIGHT_API_RETAIN_LLM_MAX_CONCURRENT=${toString retainLlmProfile.maxConcurrent}"
  ++ lib.optional (
    reflectLlmProfile.maxConcurrent != null
  ) "HINDSIGHT_API_REFLECT_LLM_MAX_CONCURRENT=${toString reflectLlmProfile.maxConcurrent}"
  ++ lib.optional usesCerebras "HINDSIGHT_API_LLM_MAX_CONCURRENT=4"
  ++ lib.optional (
    retainLlmProfile.baseUrl == cerebrasBaseUrl
  ) "HINDSIGHT_API_CONSOLIDATION_LLM_MAX_CONCURRENT=2";
  providerSecrets = lib.genAttrs requiredProviderSecrets (_: { });
  codexAuthCheck = pkgs.writeShellScript "hindsight-check-codex-auth" ''
    if ! ${pkgs.podman}/bin/podman unshare ${pkgs.coreutils}/bin/test -r ${lib.escapeShellArg codexAuthFile}; then
      echo "Hindsight Codex OAuth credentials are missing: ${codexAuthFile}" >&2
      echo "Copy a dedicated Codex auth.json to that path with mode 0600." >&2
      exit 1
    fi

    if ! ${pkgs.podman}/bin/podman unshare ${pkgs.jq}/bin/jq -e '
      .auth_mode == "chatgpt"
      and (.tokens.access_token | type == "string")
      and (.tokens.account_id | type == "string")
    ' ${lib.escapeShellArg codexAuthFile} >/dev/null; then
      echo "Hindsight Codex OAuth credentials have an incompatible schema: ${codexAuthFile}" >&2
      echo 'Expected auth_mode="chatgpt" with access_token and account_id token fields.' >&2
      exit 1
    fi
  '';
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
      default = "ghcr.io/vectorize-io/hindsight:0.8.4@sha256:2c60f233eaba8f51db31adb920a560735aaf6f314e4b63c36c73159742dfa1a7";
      description = "Hindsight standalone container image.";
    };

    postgresImage = lib.mkOption {
      type = lib.types.str;
      # renovate: docker-image
      default = "docker.io/pgvector/pgvector:0.8.5-pg18@sha256:12a379b47ad65289572ea0756efc11b7c241a6662833e8af7038cd3b73d647e0";
      description = "PostgreSQL container image with pgvector installed.";
    };

    llm = {
      retain.profile = lib.mkOption {
        type = lib.types.enum (builtins.attrNames llmProfiles);
        default = "openai-gpt-5-mini";
        description = ''
          Fixed provider and model profile for retain operations. This profile
          also supplies the global LLM fallback used by consolidation.
        '';
      };

      reflect.profile = lib.mkOption {
        type = lib.types.enum (builtins.attrNames llmProfiles);
        default = "openai-gpt-5-mini";
        description = "Fixed provider and model profile for reflect operations.";
      };
    };

    embeddings.profile = lib.mkOption {
      type = lib.types.enum (builtins.attrNames embeddingsProfiles);
      default = "openai-small";
      description = ''
        Fixed provider and model profile for Hindsight embeddings. Changing this
        after storing memories requires re-embedding or recreating vector data.
      '';
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
      isNormalUser = true;
      group = cfg.group.name;
      home = cfg.dataDir;
      createHome = false;
      shell = pkgs.shadow;
      linger = true;
      autoSubUidGidRange = true;
    };

    users.groups.${cfg.group.name} = {
      inherit (cfg.group) name;
      gid = lib.mkForce cfg.group.gid;
    };

    modules.zfs.datasets.properties = {
      "rpool/encrypted/safe/svc/hindsight"."mountpoint" = cfg.dataDir;
      "rpool/encrypted/safe/svc/hindsight"."com.sun:auto-snapshot" = "false";
    };

    systemd.tmpfiles.rules = [
      "d '${cfg.dataDir}' 0750 ${cfg.user.name} ${cfg.group.name} - -"
      "d ${codexAuthDir} 0700 :${cfg.user.name} :${cfg.group.name} -"
    ];

    sops.secrets = {
      "hindsight/postgres-password" = { };
      "hindsight/api-key" = { };
      "hindsight/control-plane-access-key" = { };
    }
    // providerSecrets;

    sops.templates = {
      "hindsight-db.env" = {
        owner = cfg.user.name;
        group = cfg.group.name;
        mode = "0400";
        content = "POSTGRES_PASSWORD=${config.sops.placeholder."hindsight/postgres-password"}\n";
      };

      "hindsight-app.env" = {
        owner = cfg.user.name;
        group = cfg.group.name;
        mode = "0400";
        content =
          lib.concatStringsSep "\n" (
            [
              "HINDSIGHT_API_DATABASE_URL=postgresql://hindsight:${
                config.sops.placeholder."hindsight/postgres-password"
              }@hindsight-db:5432/hindsight"
            ]
            ++ lib.mapAttrsToList (
              environmentVariable: secret: "${environmentVariable}=${config.sops.placeholder.${secret}}"
            ) providerSecretEnvironment
            ++ [
              "HINDSIGHT_API_TENANT_API_KEY=${config.sops.placeholder."hindsight/api-key"}"
              "HINDSIGHT_CP_DATAPLANE_API_KEY=${config.sops.placeholder."hindsight/api-key"}"
              "HINDSIGHT_CP_ACCESS_KEY=${config.sops.placeholder."hindsight/control-plane-access-key"}"
            ]
          )
          + "\n";
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
            StartLimitIntervalSec = 0;
          };
          serviceConfig = {
            ExecStartPre = [ "${pkgs.coreutils}/bin/test -r ${dbEnvironmentFile}" ];
            Restart = "on-failure";
            RestartMode = "direct";
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
            StartLimitIntervalSec = 0;
          };
          serviceConfig = {
            ExecStartPre = [
              "${pkgs.coreutils}/bin/test -r ${appEnvironmentFile}"
            ]
            ++ lib.optionals usesCodexOAuth [ codexAuthCheck ];
            Restart = "on-failure";
            RestartMode = "direct";
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
              "HINDSIGHT_API_EMBEDDINGS_PROVIDER=${embeddingsProfile.provider}"
              "${embeddingsProfile.modelEnvironment}=${embeddingsProfile.model}"
              "HINDSIGHT_API_WORKER_ID=${config.networking.hostName}"
              "HINDSIGHT_API_HOST=0.0.0.0"
              "HINDSIGHT_API_PORT=8888"
              "HINDSIGHT_CP_DATAPLANE_API_URL=http://127.0.0.1:8888"
              "HINDSIGHT_API_TENANT_EXTENSION=hindsight_api.extensions.builtin.tenant:ApiKeyTenantExtension"
            ]
            ++ providerEnvironment
            ++ lib.optionals usesCodexOAuth [
              "CODEX_HOME=${codexContainerDir}"
            ];
            Volume = lib.optionals usesCodexOAuth [
              "${codexAuthDir}:${codexContainerDir}:U"
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

    services.nginx.virtualHosts.${cfg.domain}.locations."^~ /hindsight-api/" = {
      # The trailing slash strips the public prefix before proxying to Hindsight.
      proxyPass = "http://127.0.0.1:${toString cfg.ports.api}/";
      recommendedProxySettings = true;
    };
  };
}
