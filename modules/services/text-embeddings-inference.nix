{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.services.text-embeddings-inference;
  containerName = "text-embeddings-inference";
  serviceName = "podman-${containerName}";
  firewallPorts = lib.concatStringsSep ", " (
    map toString [
      cfg.port
      cfg.prometheusPort
    ]
  );
  firewallSources = lib.concatStringsSep ", " cfg.allowedIPv4Ranges;
in
{
  options.modules.services.text-embeddings-inference = {
    enable = lib.mkEnableOption "Text Embeddings Inference reranker";

    image = lib.mkOption {
      type = lib.types.str;
      # renovate: docker-image
      default = "ghcr.io/huggingface/text-embeddings-inference:100-1.9.3@sha256:8a50d10f8192ae18f7b01124a499a6f1524d7f10b412390537f94e55a051b805";
      description = "Digest-pinned Text Embeddings Inference OCI image.";
    };

    modelId = lib.mkOption {
      type = lib.types.str;
      default = "cross-encoder/ms-marco-MiniLM-L6-v2";
      description = "Hugging Face model identifier served by the reranker.";
    };

    modelRevision = lib.mkOption {
      type = lib.types.str;
      default = "c5ee24cb16019beea0893ab7796b1df96625c6b8";
      description = "Immutable Hugging Face model revision.";
    };

    dataDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/text-embeddings-inference";
      description = "Persistent directory used for the Hugging Face model cache.";
    };

    gpuDevice = lib.mkOption {
      type = lib.types.str;
      default = "nvidia.com/gpu=0";
      description = "NVIDIA CDI device identifier exposed to the container.";
    };

    listenAddress = lib.mkOption {
      type = lib.types.str;
      description = "Host address on which the HTTP and Prometheus servers listen.";
    };

    listenInterface = lib.mkOption {
      type = lib.types.str;
      description = "Network interface from which reranker traffic is accepted.";
    };

    allowedIPv4Ranges = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      description = "IPv4 CIDRs allowed to reach the HTTP and Prometheus ports.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 8082;
      description = "HTTP API port.";
    };

    prometheusPort = lib.mkOption {
      type = lib.types.port;
      default = 9082;
      description = "Prometheus metrics port.";
    };

    autoStart = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to start the reranker automatically at boot.";
    };

    tokenizationWorkers = lib.mkOption {
      type = lib.types.ints.positive;
      default = 2;
      description = "Number of CPU workers used for tokenization.";
    };

    maxConcurrentRequests = lib.mkOption {
      type = lib.types.ints.positive;
      default = 8;
      description = "Maximum number of concurrent HTTP requests.";
    };

    maxBatchRequests = lib.mkOption {
      type = lib.types.ints.positive;
      default = 8;
      description = "Maximum number of inference requests in one GPU batch.";
    };

    maxBatchTokens = lib.mkOption {
      type = lib.types.ints.positive;
      default = 2048;
      description = "Maximum number of tokens in one GPU batch.";
    };

    maxClientBatchSize = lib.mkOption {
      type = lib.types.ints.positive;
      default = 512;
      description = "Maximum number of query-text pairs accepted in one client request.";
    };

    resources = {
      cpus = lib.mkOption {
        type = lib.types.ints.positive;
        default = 2;
        description = "Container CPU limit.";
      };

      memory = lib.mkOption {
        type = lib.types.str;
        default = "2g";
        description = "Container system-memory limit accepted by Podman.";
      };

      pids = lib.mkOption {
        type = lib.types.ints.positive;
        default = 256;
        description = "Container process limit.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = lib.hasInfix "@sha256:" cfg.image;
        message = "modules.services.text-embeddings-inference.image must be pinned by digest";
      }
      {
        assertion = cfg.modelRevision != "main";
        message = "modules.services.text-embeddings-inference.modelRevision must be immutable";
      }
      {
        assertion = lib.hasPrefix "/" cfg.dataDir;
        message = "modules.services.text-embeddings-inference.dataDir must be an absolute path";
      }
      {
        assertion = lib.hasPrefix "nvidia.com/gpu=" cfg.gpuDevice;
        message = "modules.services.text-embeddings-inference.gpuDevice must be an NVIDIA CDI identifier";
      }
      {
        assertion = cfg.listenAddress != "0.0.0.0" && cfg.listenAddress != "::";
        message = "modules.services.text-embeddings-inference.listenAddress must not be a wildcard";
      }
      {
        assertion = cfg.allowedIPv4Ranges != [ ];
        message = "modules.services.text-embeddings-inference.allowedIPv4Ranges must not be empty";
      }
      {
        assertion = cfg.port != cfg.prometheusPort;
        message = "Text Embeddings Inference HTTP and Prometheus ports must differ";
      }
    ];

    hardware.nvidia-container-toolkit.enable = true;

    virtualisation = {
      podman.enable = true;
      oci-containers = {
        backend = "podman";
        containers.${containerName} = {
          inherit (cfg) autoStart image;
          pull = "missing";
          devices = [ cfg.gpuDevice ];
          networks = [ "host" ];
          volumes = [ "${cfg.dataDir}:/data:rw" ];
          cmd = [
            "--model-id"
            cfg.modelId
            "--revision"
            cfg.modelRevision
            "--hostname"
            cfg.listenAddress
            "--port"
            (toString cfg.port)
            "--prometheus-port"
            (toString cfg.prometheusPort)
            "--dtype"
            "float16"
            "--tokenization-workers"
            (toString cfg.tokenizationWorkers)
            "--max-concurrent-requests"
            (toString cfg.maxConcurrentRequests)
            "--max-batch-requests"
            (toString cfg.maxBatchRequests)
            "--max-batch-tokens"
            (toString cfg.maxBatchTokens)
            "--max-client-batch-size"
            (toString cfg.maxClientBatchSize)
            "--disable-spans"
            "--json-output"
          ];
          podman.sdnotify = "healthy";
          extraOptions = [
            "--cpus=${toString cfg.resources.cpus}"
            "--memory=${cfg.resources.memory}"
            "--pids-limit=${toString cfg.resources.pids}"
            "--health-cmd=curl --fail --silent --show-error http://${cfg.listenAddress}:${toString cfg.port}/health >/dev/null"
            "--health-interval=30s"
            "--health-timeout=5s"
            "--health-start-period=5m"
            "--health-retries=3"
            "--health-on-failure=kill"
          ];
        };
      };
    };

    environment.persistence."/persist".directories = lib.mkIf config.modules.impermanence.enable [
      cfg.dataDir
    ];

    systemd.tmpfiles.rules = [
      "d ${cfg.dataDir} 0750 root root -"
    ];

    systemd.services.${serviceName} = {
      after = [ "nvidia-container-toolkit-cdi-generator.service" ];
      requires = [ "nvidia-container-toolkit-cdi-generator.service" ];
      unitConfig.RequiresMountsFor = [ cfg.dataDir ];
      serviceConfig = {
        RestartSec = "10s";
        TimeoutStartSec = lib.mkForce "10m";
      };
    };

    networking.firewall.extraInputRules = ''
      iifname "${cfg.listenInterface}" ip saddr { ${firewallSources} } tcp dport { ${firewallPorts} } accept comment "Text Embeddings Inference"
    '';
  };
}
