{ inputs, pkgs }:

let
  testSecrets = ./fixtures/hindsight.sops.yaml;
  latencyEnvironment = {
    HINDSIGHT_API_CONSOLIDATION_LLM_MAX_CONCURRENT = "1";
    HINDSIGHT_API_CONSOLIDATION_LLM_PARALLELISM = "1";
    HINDSIGHT_API_RERANKER_LOCAL_BUCKET_BATCHING = "true";
    HINDSIGHT_API_RERANKER_LOCAL_MAX_CONCURRENT = "1";
    HINDSIGHT_API_RETAIN_LLM_MAX_CONCURRENT = "2";
    HINDSIGHT_API_WORKER_MAX_SLOTS = "4";
  };
  expectedLatencyEnvironment = [
    "HINDSIGHT_API_CONSOLIDATION_LLM_MAX_CONCURRENT=1"
    "HINDSIGHT_API_CONSOLIDATION_LLM_PARALLELISM=1"
    "HINDSIGHT_API_RERANKER_LOCAL_BUCKET_BATCHING=true"
    "HINDSIGHT_API_RERANKER_LOCAL_MAX_CONCURRENT=1"
    "HINDSIGHT_API_RETAIN_LLM_MAX_CONCURRENT=2"
    "HINDSIGHT_API_WORKER_MAX_SLOTS=4"
  ];
  debordWorkloadEnvironment = {
    HINDSIGHT_API_CONSOLIDATION_LLM_MAX_CONCURRENT = "2";
    HINDSIGHT_API_CONSOLIDATION_LLM_PARALLELISM = "2";
    HINDSIGHT_API_RETAIN_BATCH_ENABLED = "true";
    HINDSIGHT_API_RETAIN_LLM_MAX_CONCURRENT = "2";
    HINDSIGHT_API_WORKER_MAX_SLOTS = "4";
  };
  expectedDebordWorkloadEnvironment = [
    "HINDSIGHT_API_CONSOLIDATION_LLM_MAX_CONCURRENT=2"
    "HINDSIGHT_API_CONSOLIDATION_LLM_PARALLELISM=2"
    "HINDSIGHT_API_RETAIN_BATCH_ENABLED=true"
    "HINDSIGHT_API_RETAIN_LLM_MAX_CONCURRENT=2"
    "HINDSIGHT_API_WORKER_MAX_SLOTS=4"
  ];
  testHindsightServer = pkgs.writeShellApplication {
    name = "hindsight-test-server";
    runtimeInputs = [ pkgs.busybox ];
    text = ''
      mkdir -p /tmp/hindsight-test/api/v1/default /tmp/hindsight-test/control-plane/api
      printf '%s\n' healthy > /tmp/hindsight-test/api/health
      printf '%s\n' '{"api_version":"test"}' > /tmp/hindsight-test/api/version
      printf '%s\n' '{"banks":[]}' > /tmp/hindsight-test/api/v1/default/banks
      printf '%s\n' hindsight-control-plane > /tmp/hindsight-test/control-plane/index.html
      printf '%s\n' control-plane-api > /tmp/hindsight-test/control-plane/api/health
      httpd -f -p 8888 -h /tmp/hindsight-test/api &
      httpd -f -p 9999 -h /tmp/hindsight-test/control-plane &
      wait
    '';
  };

  testHindsightImage = pkgs.dockerTools.buildLayeredImage {
    name = "hindsight-test";
    tag = "latest";
    contents = [
      pkgs.busybox
      pkgs.curl
      testHindsightServer
    ];
    config = {
      Entrypoint = [ "${testHindsightServer}/bin/hindsight-test-server" ];
      Env = [
        "PATH=${
          pkgs.lib.makeBinPath [
            pkgs.busybox
            pkgs.curl
          ]
        }"
      ];
    };
  };

  testPostgresServer = pkgs.writeShellApplication {
    name = "hindsight-test-postgres";
    runtimeInputs = [
      pkgs.busybox
      pkgs.gnugrep
    ];
    text = ''
      test "$POSTGRES_USER" = hindsight
      test "$POSTGRES_DB" = hindsight
      test "$POSTGRES_PASSWORD" = test-postgres-password
      grep -Fxq 'CREATE EXTENSION IF NOT EXISTS vector;' \
        /docker-entrypoint-initdb.d/10-hindsight.sql
      mkdir -p /var/lib/postgresql/18/docker
      printf '%s\n' initialized > /var/lib/postgresql/18/docker/hindsight-test
      exec sleep infinity
    '';
  };

  testPgReady = pkgs.writeShellApplication {
    name = "pg_isready";
    text = "exit 0";
  };

  testPostgresImage = pkgs.dockerTools.buildLayeredImage {
    name = "hindsight-postgres-test";
    tag = "latest";
    contents = [
      pkgs.busybox
      pkgs.gnugrep
      testPgReady
      testPostgresServer
    ];
    config = {
      Entrypoint = [ "${testPostgresServer}/bin/hindsight-test-postgres" ];
      Env = [
        "PATH=${
          pkgs.lib.makeBinPath [
            pkgs.busybox
            pkgs.gnugrep
            testPgReady
          ]
        }"
      ];
    };
  };

  profileConfig =
    {
      retainLlmProfile ? "openai-gpt-5-mini",
      consolidationLlmProfile ? null,
      reflectLlmProfile ? "openai-gpt-5-mini",
      embeddingsProfile ? "openai-small",
      extraEnvironment ? { },
    }:
    (inputs.nixpkgs.lib.nixosSystem {
      system = pkgs.stdenv.hostPlatform.system;
      modules = [
        inputs.impermanence.nixosModules.impermanence
        inputs.quadlet-nix2.nixosModules.default
        inputs.sops-nix.nixosModules.sops
        ../modules/zfs-attrs.nix
        ../modules/services/ingress.nix
        ../modules/services/hindsight.nix
        {
          networking.hostName = "debord";
          system.stateVersion = "26.05";

          fileSystems."/persist" = {
            device = "tmpfs";
            fsType = "tmpfs";
            neededForBoot = true;
          };

          modules.services.hindsight = {
            enable = true;
            domain = "hindsight.private.invalid";
            acmeHost = "private.invalid";
            llm = {
              retain.profile = retainLlmProfile;
              reflect.profile = reflectLlmProfile;
            }
            // inputs.nixpkgs.lib.optionalAttrs (consolidationLlmProfile != null) {
              consolidation.profile = consolidationLlmProfile;
            };
            embeddings.profile = embeddingsProfile;
            inherit extraEnvironment;
          };

          sops = {
            defaultSopsFile = testSecrets;
            age.keyFile = "/etc/sops/age/keys.txt";
          };
        }
      ];
    }).config;

  openaiCerebrasConfig = profileConfig {
    retainLlmProfile = "openai-gpt-4.1-nano";
    reflectLlmProfile = "cerebras-gpt-oss-120b";
    embeddingsProfile = "openai-large";
  };
  cerebrasCodexConfig = profileConfig {
    retainLlmProfile = "cerebras-gemma-4-31b";
    reflectLlmProfile = "openai-codex-gpt-5.4-mini";
    embeddingsProfile = "openai-codex-small";
  };
  codexGeminiConfig = profileConfig {
    retainLlmProfile = "openai-codex-gpt-5.4-mini";
    reflectLlmProfile = "gemini-3.1-flash-lite";
    embeddingsProfile = "openai-large";
  };
  nanoCodexConfig = profileConfig {
    retainLlmProfile = "openai-gpt-4.1-nano";
    consolidationLlmProfile = "openai-codex-gpt-5.4-mini";
    reflectLlmProfile = "openai-codex-gpt-5.4-mini";
    embeddingsProfile = "local-bge-small-en-v1.5";
    extraEnvironment = debordWorkloadEnvironment;
  };
  cerebrasConsolidationConfig = profileConfig {
    retainLlmProfile = "openai-gpt-4.1-nano";
    consolidationLlmProfile = "cerebras-gemma-4-31b";
    reflectLlmProfile = "openai-codex-gpt-5.4-mini";
    embeddingsProfile = "openai-large";
  };
in
pkgs.testers.runNixOSTest {
  name = "hindsight-rootless";

  nodes.machine =
    {
      config,
      lib,
      options,
      ...
    }:
    let
      app = config.virtualisation.quadlet.containers.hindsight;
      database = config.virtualisation.quadlet.containers.hindsight-db;
      network = config.virtualisation.quadlet.networks.hindsight;
      volume = config.virtualisation.quadlet.volumes.hindsight-db-data;
      ingress = config.modules.services.ingress.virtualHosts."hindsight.private.invalid";
      hindsightDataset = config.modules.zfs.datasets.properties."rpool/encrypted/safe/svc/hindsight";
      appTemplate = config.sops.templates."hindsight-app.env";
      dbTemplate = config.sops.templates."hindsight-db.env";
      openaiCerebrasApp = openaiCerebrasConfig.virtualisation.quadlet.containers.hindsight;
      cerebrasCodexApp = cerebrasCodexConfig.virtualisation.quadlet.containers.hindsight;
      codexGeminiApp = codexGeminiConfig.virtualisation.quadlet.containers.hindsight;
      nanoCodexApp = nanoCodexConfig.virtualisation.quadlet.containers.hindsight;
      nanoCodexTemplate = nanoCodexConfig.sops.templates."hindsight-app.env";
      cerebrasConsolidationApp = cerebrasConsolidationConfig.virtualisation.quadlet.containers.hindsight;
      cerebrasConsolidationTemplate = cerebrasConsolidationConfig.sops.templates."hindsight-app.env";
    in
    {
      imports = [
        inputs.impermanence.nixosModules.impermanence
        inputs.quadlet-nix2.nixosModules.default
        inputs.sops-nix.nixosModules.sops
        ../modules/zfs-attrs.nix
        ../modules/services/ingress.nix
        ../modules/services/hindsight.nix
      ];

      networking.hostName = "debord";
      system.stateVersion = "26.05";

      fileSystems."/persist" = {
        device = "tmpfs";
        fsType = "tmpfs";
        neededForBoot = true;
      };

      modules.services.hindsight = {
        enable = true;
        domain = "hindsight.private.invalid";
        acmeHost = "private.invalid";
        image = "docker-archive:${testHindsightImage}";
        postgresImage = "docker-archive:${testPostgresImage}";
        llm = {
          retain.profile = "openai-gpt-5-mini";
          reflect.profile = "openai-gpt-5-mini";
        };
        embeddings.profile = "openai-small";
        extraEnvironment = latencyEnvironment;
      };

      services.nginx = {
        enable = true;
        virtualHosts."hindsight.private.invalid".locations."/" = {
          proxyPass = "http://127.0.0.1:9999";
          recommendedProxySettings = true;
        };
      };

      sops = {
        defaultSopsFile = testSecrets;
        age.keyFile = "/etc/sops/age/keys.txt";
      };

      # This deliberately committed test-only key is split so secret scanning still
      # rejects any complete age private key accidentally added to the repository.
      environment.etc."sops/age/keys.txt".text =
        "AGE-SECRET-" + "KEY-17234342FN8EHCK6G4REV520TDFALHLTWDV9YHYLK72XEY3VY2XRS4JWM6A\n";

      assertions = [
        {
          assertion = lib.all (object: object.uid == 3020) [
            app
            database
            network
            volume
          ];
          message = "Every Hindsight Quadlet object must run as the rootless service user.";
        }
        {
          assertion =
            config.users.users.hindsight.isNormalUser
            && config.users.users.hindsight.autoSubUidGidRange
            && config.users.users.hindsight.shell == pkgs.shadow;
          message = "The non-login Hindsight user must receive subordinate IDs for rootless Podman.";
        }
        {
          assertion =
            database.unitConfig.StartLimitIntervalSec == 0
            && database.serviceConfig.RestartMode == "direct"
            && app.unitConfig.StartLimitIntervalSec == 0
            && app.serviceConfig.RestartMode == "direct";
          message = "Hindsight Quadlets must keep retrying transient image-pull and dependency failures.";
        }
        {
          assertion = lib.hasInfix "ConditionUser=3020" app.text;
          message = "The generated Hindsight Quadlet must be a UID-gated user unit.";
        }
        {
          assertion = lib.hasInfix "PublishPort=127.0.0.1:8888:8888" app.text;
          message = "The Hindsight API must bind only to loopback.";
        }
        {
          assertion = lib.hasInfix "PublishPort=127.0.0.1:9999:9999" app.text;
          message = "The Hindsight control plane must bind only to loopback.";
        }
        {
          assertion = ingress.forwardAuth == false;
          message = "Hindsight must use its native authentication, not Authentik forward auth.";
        }
        {
          assertion = config.modules.services.ingress.domains == { };
          message = "Hindsight must not be added to an external ingress tunnel.";
        }
        {
          assertion =
            config.services.nginx.virtualHosts."hindsight.private.invalid".locations."^~ /hindsight-api/".proxyPass
            == "http://127.0.0.1:8888/";
          message = "The prefixed Hindsight API route must strip its prefix and target the loopback API port.";
        }
        {
          assertion = lib.elem "HINDSIGHT_API_TENANT_EXTENSION=hindsight_api.extensions.builtin.tenant:ApiKeyTenantExtension" app.containerConfig.Environment;
          message = "The Hindsight API-key tenant extension must be enabled.";
        }
        {
          assertion =
            lib.elem "HINDSIGHT_API_LLM_PROVIDER=openai" app.containerConfig.Environment
            && lib.elem "HINDSIGHT_API_LLM_MODEL=gpt-5-mini" app.containerConfig.Environment
            && lib.elem "HINDSIGHT_API_RETAIN_LLM_PROVIDER=openai" app.containerConfig.Environment
            && lib.elem "HINDSIGHT_API_RETAIN_LLM_MODEL=gpt-5-mini" app.containerConfig.Environment
            && lib.elem "HINDSIGHT_API_CONSOLIDATION_LLM_PROVIDER=openai" app.containerConfig.Environment
            && lib.elem "HINDSIGHT_API_CONSOLIDATION_LLM_MODEL=gpt-5-mini" app.containerConfig.Environment
            && lib.elem "HINDSIGHT_API_REFLECT_LLM_PROVIDER=openai" app.containerConfig.Environment
            && lib.elem "HINDSIGHT_API_REFLECT_LLM_MODEL=gpt-5-mini" app.containerConfig.Environment
            && lib.elem "HINDSIGHT_API_EMBEDDINGS_PROVIDER=openai" app.containerConfig.Environment
            && lib.elem "HINDSIGHT_API_EMBEDDINGS_OPENAI_MODEL=text-embedding-3-small" app.containerConfig.Environment;
          message = "Explicit retain, consolidation, reflect, and embeddings profiles must map to their provider settings.";
        }
        {
          assertion = lib.all (
            entry: lib.elem entry app.containerConfig.Environment
          ) expectedLatencyEnvironment;
          message = "Extra Hindsight environment values must render in the generated container.";
        }
        {
          assertion =
            builtins.hasAttr "hindsight/openai-api-key" config.sops.secrets
            &&
              builtins.length (
                lib.filter (name: name == "hindsight/openai-api-key") (builtins.attrNames config.sops.secrets)
              ) == 1
            && lib.hasInfix "HINDSIGHT_API_LLM_API_KEY=" appTemplate.content
            && lib.hasInfix "HINDSIGHT_API_RETAIN_LLM_API_KEY=" appTemplate.content
            && lib.hasInfix "HINDSIGHT_API_CONSOLIDATION_LLM_API_KEY=" appTemplate.content
            && lib.hasInfix "HINDSIGHT_API_REFLECT_LLM_API_KEY=" appTemplate.content
            && lib.hasInfix "HINDSIGHT_API_EMBEDDINGS_OPENAI_API_KEY=" appTemplate.content;
          message = "Profiles using one provider must share one declared SOPS secret.";
        }
        {
          assertion = lib.all (entry: lib.elem entry openaiCerebrasApp.containerConfig.Environment) [
            "HINDSIGHT_API_LLM_PROVIDER=openai"
            "HINDSIGHT_API_LLM_MODEL=gpt-4.1-nano"
            "HINDSIGHT_API_RETAIN_LLM_PROVIDER=openai"
            "HINDSIGHT_API_RETAIN_LLM_MODEL=gpt-4.1-nano"
            "HINDSIGHT_API_REFLECT_LLM_PROVIDER=openai"
            "HINDSIGHT_API_REFLECT_LLM_MODEL=gpt-oss-120b"
            "HINDSIGHT_API_REFLECT_LLM_BASE_URL=https://api.cerebras.ai/v1"
            "HINDSIGHT_API_LLM_MAX_CONCURRENT=4"
            "HINDSIGHT_API_REFLECT_LLM_MAX_CONCURRENT=2"
            "HINDSIGHT_API_EMBEDDINGS_PROVIDER=openai"
            "HINDSIGHT_API_EMBEDDINGS_OPENAI_MODEL=text-embedding-3-large"
          ];
          message = "Retain and reflect must select independent OpenAI and Cerebras profiles.";
        }
        {
          assertion =
            builtins.hasAttr "hindsight/cerebras-api-key" openaiCerebrasConfig.sops.secrets
            && builtins.hasAttr "hindsight/openai-api-key" openaiCerebrasConfig.sops.secrets
            &&
              lib.hasInfix "HINDSIGHT_API_LLM_API_KEY="
                openaiCerebrasConfig.sops.templates."hindsight-app.env".content
            &&
              lib.hasInfix "HINDSIGHT_API_RETAIN_LLM_API_KEY="
                openaiCerebrasConfig.sops.templates."hindsight-app.env".content
            &&
              lib.hasInfix "HINDSIGHT_API_REFLECT_LLM_API_KEY="
                openaiCerebrasConfig.sops.templates."hindsight-app.env".content;
          message = "Independent OpenAI and Cerebras operation profiles must render both provider keys.";
        }
        {
          assertion =
            lib.all (entry: lib.elem entry cerebrasCodexApp.containerConfig.Environment) [
              "HINDSIGHT_API_LLM_PROVIDER=openai"
              "HINDSIGHT_API_LLM_MODEL=gemma-4-31b"
              "HINDSIGHT_API_LLM_BASE_URL=https://api.cerebras.ai/v1"
              "HINDSIGHT_API_RETAIN_LLM_PROVIDER=openai"
              "HINDSIGHT_API_RETAIN_LLM_MODEL=gemma-4-31b"
              "HINDSIGHT_API_RETAIN_LLM_BASE_URL=https://api.cerebras.ai/v1"
              "HINDSIGHT_API_REFLECT_LLM_PROVIDER=openai-codex"
              "HINDSIGHT_API_REFLECT_LLM_MODEL=gpt-5.4-mini"
              "HINDSIGHT_API_EMBEDDINGS_PROVIDER=openai-codex"
              "HINDSIGHT_API_EMBEDDINGS_OPENAI_MODEL=text-embedding-3-small"
              "CODEX_HOME=/var/lib/hindsight/codex"
            ]
            &&
              cerebrasCodexApp.containerConfig.Volume == [
                "/var/lib/hindsight/codex:/var/lib/hindsight/codex:U"
              ]
            &&
              builtins.length cerebrasCodexApp.serviceConfig.ExecStartPre
              == builtins.length app.serviceConfig.ExecStartPre + 1
            && builtins.hasAttr "hindsight/cerebras-api-key" cerebrasCodexConfig.sops.secrets
            && !(builtins.hasAttr "hindsight/openai-api-key" cerebrasCodexConfig.sops.secrets)
            && !(lib.hasInfix "HINDSIGHT_API_REFLECT_LLM_API_KEY="
              cerebrasCodexConfig.sops.templates."hindsight-app.env".content
            )
            && !(lib.hasInfix "HINDSIGHT_API_EMBEDDINGS_OPENAI_API_KEY="
              cerebrasCodexConfig.sops.templates."hindsight-app.env".content
            );
          message = "The Codex gpt-5.4-mini reflect profile must use dedicated OAuth state without an API key.";
        }
        {
          assertion =
            lib.all (entry: lib.elem entry codexGeminiApp.containerConfig.Environment) [
              "HINDSIGHT_API_LLM_PROVIDER=openai-codex"
              "HINDSIGHT_API_LLM_MODEL=gpt-5.4-mini"
              "HINDSIGHT_API_RETAIN_LLM_PROVIDER=openai-codex"
              "HINDSIGHT_API_RETAIN_LLM_MODEL=gpt-5.4-mini"
              "HINDSIGHT_API_REFLECT_LLM_PROVIDER=gemini"
              "HINDSIGHT_API_REFLECT_LLM_MODEL=gemini-3.1-flash-lite"
              "HINDSIGHT_API_EMBEDDINGS_PROVIDER=openai"
              "HINDSIGHT_API_EMBEDDINGS_OPENAI_MODEL=text-embedding-3-large"
              "CODEX_HOME=/var/lib/hindsight/codex"
            ]
            && builtins.hasAttr "hindsight/gemini-api-key" codexGeminiConfig.sops.secrets
            && builtins.hasAttr "hindsight/openai-api-key" codexGeminiConfig.sops.secrets
            && !(lib.hasInfix "HINDSIGHT_API_LLM_API_KEY="
              codexGeminiConfig.sops.templates."hindsight-app.env".content
            )
            && !(lib.hasInfix "HINDSIGHT_API_RETAIN_LLM_API_KEY="
              codexGeminiConfig.sops.templates."hindsight-app.env".content
            );
          message = "The Codex gpt-5.4-mini retain profile must coexist with independent reflect and embeddings profiles.";
        }
        {
          assertion =
            lib.all (entry: lib.elem entry nanoCodexApp.containerConfig.Environment) (
              [
                "HINDSIGHT_API_LLM_PROVIDER=openai"
                "HINDSIGHT_API_LLM_MODEL=gpt-4.1-nano"
                "HINDSIGHT_API_RETAIN_LLM_PROVIDER=openai"
                "HINDSIGHT_API_RETAIN_LLM_MODEL=gpt-4.1-nano"
                "HINDSIGHT_API_CONSOLIDATION_LLM_PROVIDER=openai-codex"
                "HINDSIGHT_API_CONSOLIDATION_LLM_MODEL=gpt-5.4-mini"
                "HINDSIGHT_API_REFLECT_LLM_PROVIDER=openai-codex"
                "HINDSIGHT_API_REFLECT_LLM_MODEL=gpt-5.4-mini"
                "CODEX_HOME=/var/lib/hindsight/codex"
              ]
              ++ expectedDebordWorkloadEnvironment
            )
            && !(lib.any (lib.hasInfix "cerebras.ai") nanoCodexApp.containerConfig.Environment)
            && builtins.hasAttr "hindsight/openai-api-key" nanoCodexConfig.sops.secrets
            && lib.hasInfix "HINDSIGHT_API_LLM_API_KEY=" nanoCodexTemplate.content
            && lib.hasInfix "HINDSIGHT_API_RETAIN_LLM_API_KEY=" nanoCodexTemplate.content
            && !(lib.hasInfix "HINDSIGHT_API_CONSOLIDATION_LLM_API_KEY=" nanoCodexTemplate.content)
            && !(lib.hasInfix "HINDSIGHT_API_REFLECT_LLM_API_KEY=" nanoCodexTemplate.content);
          message = "Retain must support OpenAI Batch API credentials while consolidation and reflect use Codex OAuth.";
        }
        {
          assertion =
            lib.all (entry: lib.elem entry cerebrasConsolidationApp.containerConfig.Environment) [
              "HINDSIGHT_API_CONSOLIDATION_LLM_PROVIDER=openai"
              "HINDSIGHT_API_CONSOLIDATION_LLM_MODEL=gemma-4-31b"
              "HINDSIGHT_API_CONSOLIDATION_LLM_BASE_URL=https://api.cerebras.ai/v1"
              "HINDSIGHT_API_CONSOLIDATION_LLM_MAX_CONCURRENT=2"
            ]
            && builtins.hasAttr "hindsight/openai-api-key" cerebrasConsolidationConfig.sops.secrets
            && builtins.hasAttr "hindsight/cerebras-api-key" cerebrasConsolidationConfig.sops.secrets
            && lib.hasInfix "HINDSIGHT_API_CONSOLIDATION_LLM_API_KEY=" cerebrasConsolidationTemplate.content;
          message = "A distinct consolidation profile must render its provider, model, base URL, concurrency, and secret.";
        }
        {
          assertion =
            hindsightDataset.mountpoint == "/var/lib/hindsight"
            && hindsightDataset."com.sun:auto-snapshot" == "false"
            && lib.elem "d '/var/lib/hindsight' 0750 hindsight hindsight - -" config.systemd.tmpfiles.rules;
          message = "Hindsight state must use an owned, dedicated ZFS dataset.";
        }
        {
          assertion = lib.elem "d /var/lib/hindsight/codex 0700 :hindsight :hindsight -" config.systemd.tmpfiles.rules;
          message = "The Codex credential directory must preserve Podman's remapped ownership on later activations.";
        }
        {
          assertion =
            lib.hasInfix "HINDSIGHT_API_TENANT_API_KEY=" appTemplate.content
            && lib.hasInfix "HINDSIGHT_CP_DATAPLANE_API_KEY=" appTemplate.content
            && lib.hasInfix "HINDSIGHT_CP_ACCESS_KEY=" appTemplate.content;
          message = "The Hindsight native-auth secrets must be rendered for both API and control plane.";
        }
        {
          assertion =
            appTemplate.owner == "hindsight"
            && appTemplate.group == "hindsight"
            && appTemplate.mode == "0400"
            && dbTemplate.owner == "hindsight"
            && dbTemplate.group == "hindsight"
            && dbTemplate.mode == "0400";
          message = "Hindsight environment files must be private to the rootless service user.";
        }
        {
          assertion =
            options.modules.services.hindsight.image.default
            == "ghcr.io/vectorize-io/hindsight:0.8.6@sha256:ffa391a77284e49f6b55e32c86f33529ac4257831407b14038a72b6a0a232039";
          message = "The production Hindsight image must remain pinned.";
        }
        {
          assertion =
            options.modules.services.hindsight.postgresImage.default
            == "docker.io/pgvector/pgvector:0.8.5-pg18@sha256:12a379b47ad65289572ea0756efc11b7c241a6662833e8af7038cd3b73d647e0";
          message = "The production pgvector image must remain pinned.";
        }
      ];
    };

  testScript = ''
    import json

    user = "hindsight"

    machine.start()
    machine.wait_for_unit("multi-user.target")
    machine.wait_for_unit("default.target", user=user)
    machine.succeed(
        "systemctl show -p Result --value sops-install-secrets.service | grep -Fxq success"
    )
    machine.succeed(
        "test $(stat -c '%U:%G:%a' /run/secrets/rendered/hindsight-app.env) = hindsight:hindsight:400"
    )
    machine.succeed("grep -Eq '^hindsight:[0-9]+:65536$' /etc/subuid")
    machine.succeed("grep -Eq '^hindsight:[0-9]+:65536$' /etc/subgid")
    machine.wait_for_unit("hindsight-db.service", user=user, timeout=120)
    machine.wait_for_unit("hindsight.service", user=user, timeout=120)
    machine.wait_for_open_port(8888)
    machine.wait_for_open_port(9999)
    machine.wait_for_unit("nginx.service")
    machine.wait_for_open_port(80)

    assert "healthy" in machine.succeed("curl -fsS http://127.0.0.1:8888/health")
    assert "hindsight-control-plane" in machine.succeed("curl -fsS http://127.0.0.1:9999/")
    nginx_curl = "curl -fsS -H 'Host: hindsight.private.invalid' http://127.0.0.1"
    assert "hindsight-control-plane" in machine.succeed(f"{nginx_curl}/")
    assert "control-plane-api" in machine.succeed(f"{nginx_curl}/api/health")
    assert "healthy" in machine.succeed(f"{nginx_curl}/hindsight-api/health")
    assert '"api_version":"test"' in machine.succeed(f"{nginx_curl}/hindsight-api/version")
    assert '"banks":[]' in machine.succeed(f"{nginx_curl}/hindsight-api/v1/default/banks")

    machine.succeed("ss -ltn | grep -q '127.0.0.1:8888'")
    machine.succeed("ss -ltn | grep -q '127.0.0.1:9999'")
    machine.fail("ss -ltn | grep -Eq '0\\.0\\.0\\.0:(8888|9999)'")

    root_containers = json.loads(machine.succeed("podman ps --format json"))
    assert root_containers == [], f"Expected no root-owned containers, got: {root_containers}"

    rootless_containers = json.loads(
        machine.succeed("sudo -u hindsight -- podman ps --format json")
    )
    names = sorted(container["Names"][0] for container in rootless_containers)
    assert names == ["hindsight", "hindsight-db"], f"Unexpected rootless containers: {names}"

    environment = machine.succeed(
        "sudo -u hindsight -- podman inspect hindsight "
        "--format '{{range .Config.Env}}{{println .}}{{end}}'"
    )
    for expected in [
        "HINDSIGHT_API_LLM_PROVIDER=openai",
        "HINDSIGHT_API_LLM_MODEL=gpt-5-mini",
        "HINDSIGHT_API_EMBEDDINGS_PROVIDER=openai",
        "HINDSIGHT_API_EMBEDDINGS_OPENAI_MODEL=text-embedding-3-small",
        "HINDSIGHT_API_EMBEDDINGS_OPENAI_API_KEY=test-openai-key",
        "HINDSIGHT_API_VECTOR_EXTENSION=pgvector",
        "HINDSIGHT_API_TENANT_EXTENSION=hindsight_api.extensions.builtin.tenant:ApiKeyTenantExtension",
        "HINDSIGHT_API_TENANT_API_KEY=test-api-key",
        "HINDSIGHT_CP_DATAPLANE_API_KEY=test-api-key",
        "HINDSIGHT_CP_ACCESS_KEY=test-control-plane-key",
    ]:
        assert expected in environment, f"Missing Hindsight environment setting: {expected}"

    for expected in ${builtins.toJSON expectedLatencyEnvironment}:
        assert expected in environment, f"Missing Hindsight latency setting: {expected}"

    machine.succeed(
        "sudo -u hindsight -- podman exec hindsight-db "
        "grep -Fxq initialized /var/lib/postgresql/18/docker/hindsight-test"
    )

    volume = json.loads(
        machine.succeed("sudo -u hindsight -- podman volume inspect hindsight-db-data")
    )[0]
    assert volume["Mountpoint"].startswith("/var/lib/hindsight/"), volume["Mountpoint"]
  '';
}
