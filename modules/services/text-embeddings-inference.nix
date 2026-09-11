{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.services.text-embeddings-inference;

  # Preserve the existing reranker container identity during the migration to instances.
  containerNameFor =
    name:
    if name == "reranker" then "text-embeddings-inference" else "text-embeddings-inference-${name}";

  instanceType = lib.types.submodule {
    options = {
      image = lib.mkOption {
        type = lib.types.str;
        # renovate: docker-image
        default = "ghcr.io/huggingface/text-embeddings-inference:89-1.9.3@sha256:e47e625ced2385d3dbfdee79ba0380204578e0b27ef1a926783f9b3486aaf109";
        description = "Digest-pinned Text Embeddings Inference OCI image.";
      };

      modelId = lib.mkOption {
        type = lib.types.str;
        default = "cross-encoder/ms-marco-MiniLM-L6-v2";
        description = "Hugging Face model identifier served by this instance.";
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
        description = "Network interface from which this instance's traffic is accepted.";
      };

      allowedIPv4Ranges = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "IPv4 CIDRs allowed to reach this instance's HTTP and Prometheus ports.";
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
        description = "Whether to start this instance automatically at boot.";
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
        description = "Maximum number of inputs accepted in one client request.";
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
  };
in
{
  options.modules.services.text-embeddings-inference = {
    enable = lib.mkEnableOption "Text Embeddings Inference instances";

    instances = lib.mkOption {
      type = lib.types.attrsOf instanceType;
      default = { };
      description = "Named Text Embeddings Inference instances.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.instances != { };
        message = "modules.services.text-embeddings-inference.instances must not be empty";
      }
    ]
    ++ lib.concatLists (
      lib.mapAttrsToList (name: instanceCfg: [
        {
          assertion = lib.hasInfix "@sha256:" instanceCfg.image;
          message = "modules.services.text-embeddings-inference.instances.${name}.image must be pinned by digest";
        }
        {
          assertion = instanceCfg.modelRevision != "main";
          message = "modules.services.text-embeddings-inference.instances.${name}.modelRevision must be immutable";
        }
        {
          assertion = lib.hasPrefix "/" instanceCfg.dataDir;
          message = "modules.services.text-embeddings-inference.instances.${name}.dataDir must be an absolute path";
        }
        {
          assertion = lib.hasPrefix "nvidia.com/gpu=" instanceCfg.gpuDevice;
          message = "modules.services.text-embeddings-inference.instances.${name}.gpuDevice must be an NVIDIA CDI identifier";
        }
        {
          assertion = instanceCfg.listenAddress != "0.0.0.0" && instanceCfg.listenAddress != "::";
          message = "modules.services.text-embeddings-inference.instances.${name}.listenAddress must not be a wildcard";
        }
        {
          assertion = instanceCfg.allowedIPv4Ranges != [ ];
          message = "modules.services.text-embeddings-inference.instances.${name}.allowedIPv4Ranges must not be empty";
        }
        {
          assertion = instanceCfg.port != instanceCfg.prometheusPort;
          message = "Text Embeddings Inference HTTP and Prometheus ports must differ for instance ${name}";
        }
      ]) cfg.instances
    );

    hardware.nvidia-container-toolkit.enable = true;

    virtualisation = {
      podman.enable = true;
      oci-containers = {
        backend = "podman";
        containers = lib.mapAttrs' (
          name: instanceCfg:
          let
            containerName = containerNameFor name;
          in
          lib.nameValuePair containerName {
            inherit (instanceCfg) autoStart image;
            devices = [ instanceCfg.gpuDevice ];
            networks = [ "host" ];
            volumes = [ "${instanceCfg.dataDir}:/data:rw" ];
            cmd = [
              "--model-id"
              instanceCfg.modelId
              "--revision"
              instanceCfg.modelRevision
              "--hostname"
              instanceCfg.listenAddress
              "--port"
              (toString instanceCfg.port)
              "--prometheus-port"
              (toString instanceCfg.prometheusPort)
              "--dtype"
              "float16"
              "--tokenization-workers"
              (toString instanceCfg.tokenizationWorkers)
              "--max-concurrent-requests"
              (toString instanceCfg.maxConcurrentRequests)
              "--max-batch-requests"
              (toString instanceCfg.maxBatchRequests)
              "--max-batch-tokens"
              (toString instanceCfg.maxBatchTokens)
              "--max-client-batch-size"
              (toString instanceCfg.maxClientBatchSize)
              "--disable-spans"
              "--json-output"
            ];
            podman.sdnotify = "healthy";
            extraOptions = [
              "--cpus=${toString instanceCfg.resources.cpus}"
              "--memory=${instanceCfg.resources.memory}"
              "--pids-limit=${toString instanceCfg.resources.pids}"
              "--health-cmd=curl --fail --silent --show-error http://${instanceCfg.listenAddress}:${toString instanceCfg.port}/health >/dev/null"
              "--health-interval=30s"
              "--health-timeout=5s"
              "--health-start-period=5m"
              "--health-retries=3"
              "--health-on-failure=kill"
            ];
          }
        ) cfg.instances;
      };
    };

    environment.persistence."/persist".directories = lib.mkIf config.modules.impermanence.enable (
      lib.mapAttrsToList (_: instanceCfg: instanceCfg.dataDir) cfg.instances
    );

    systemd.tmpfiles.rules = lib.mapAttrsToList (
      _: instanceCfg: "d ${instanceCfg.dataDir} 0750 root root -"
    ) cfg.instances;

    systemd.services = lib.mapAttrs' (
      name: instanceCfg:
      let
        containerName = containerNameFor name;
      in
      lib.nameValuePair "podman-${containerName}" {
        after = [ "nvidia-container-toolkit-cdi-generator.service" ];
        requires = [ "nvidia-container-toolkit-cdi-generator.service" ];
        unitConfig.RequiresMountsFor = [ instanceCfg.dataDir ];
        serviceConfig = {
          RestartSec = "10s";
          TimeoutStartSec = lib.mkForce "10m";
        };
      }
    ) cfg.instances;

    networking.firewall.extraInputRules = lib.concatStringsSep "\n" (
      lib.mapAttrsToList (
        _: instanceCfg:
        let
          firewallPorts = lib.concatStringsSep ", " (
            map toString [
              instanceCfg.port
              instanceCfg.prometheusPort
            ]
          );
          firewallSources = lib.concatStringsSep ", " instanceCfg.allowedIPv4Ranges;
        in
        ''
          iifname "${instanceCfg.listenInterface}" ip saddr { ${firewallSources} } tcp dport { ${firewallPorts} } accept comment "Text Embeddings Inference"
        ''
      ) cfg.instances
    );
  };
}
