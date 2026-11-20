{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.dev.llms;
  models = import ../../modules/dev/llms/models.nix;
  agentRuntimeGroups = [
    "node_runtime"
    "rust_runtime"
    "python_runtime"
    {
      name = "user_caches_macos";
      when = "macos";
    }
    {
      name = "user_caches_linux";
      when = "linux";
    }
    {
      name = "linux_sysfs_read";
      when = "linux";
    }
    "nix_runtime"
    "git_config"
    "unlink_protection"
  ];
  piRuntimeGroups = agentRuntimeGroups ++ [ "java_runtime" ];

  providerCatalog = {
    openrouter.route = {
      upstream = if cfg.proxy.enable then "${cfg.proxy.url}/v1" else "https://openrouter.ai/api/v1";
      credential_key =
        if cfg.proxy.enable then cfg.proxy.credential else "op://LLM/OpenRouter/credential";
      env_var = "OPENROUTER_API_KEY";
      inject_header = "Authorization";
      credential_format = "Bearer {}";
    };

    openai.route = {
      upstream = "https://api.openai.com/v1";
      credential_key = "op://LLM/OpenAI-quine/credential";
      env_var = "OPENAI_API_KEY";
      inject_header = "Authorization";
      credential_format = "Bearer {}";
    };

    anthropic.route = {
      upstream = "https://api.anthropic.com";
      credential_key = "op://LLM/Anthropic-quine/credential";
      env_var = "ANTHROPIC_API_KEY";
      inject_header = "x-api-key";
      credential_format = "{}";
    };

    gemini = {
      route = {
        upstream = "https://generativelanguage.googleapis.com";
        credential_key = "op://LLM/GoogleAIStudio-quine/credential";
        env_var = "GEMINI_API_KEY";
        inject_header = "x-goog-api-key";
        credential_format = "{}";
      };
      envCredentials = {
        "op://LLM/GoogleAIStudio-quine/GOOGLE_CLOUD_PROJECT" = "GOOGLE_CLOUD_PROJECT";
      };
    };

    cerebras.route = {
      upstream = "https://api.cerebras.ai/v1";
      credential_key = "op://LLM/Cerebras-quine/credential";
      env_var = "CEREBRAS_API_KEY";
      inject_header = "Authorization";
      credential_format = "Bearer {}";
    };

    mistral.route = {
      upstream = "https://api.mistral.ai/v1";
      credential_key = "op://LLM/Mistral-quine/credential";
      env_var = "MISTRAL_API_KEY";
      inject_header = "Authorization";
      credential_format = "Bearer {}";
    };

    elevenlabs.route = {
      upstream = "https://api.elevenlabs.io";
      credential_key = "op://LLM/ElevenLabs-quine/credential";
      env_var = "ELEVENLABS_API_KEY";
      inject_header = "xi-api-key";
      credential_format = "{}";
    };

    hindsight = {
      route = {
        upstream = "https://Hindsight/hindsight-api";
        credential_key = "op://Private/Hindsight/access-key";
        env_var = "HINDSIGHT_API_KEY";
        inject_header = "Authorization";
        credential_format = "Bearer {}";
      };
      envCredentials = {
        "op://Private/Hindsight/api-url" = "HINDSIGHT_API_URL";
      };
    };
  };

  mkProfile =
    {
      profile,
      providers ? lib.attrNames providerCatalog,
    }:
    let
      selectedProviders = lib.getAttrs providers providerCatalog;
      envCredentials = lib.foldl' (result: provider: result // (provider.envCredentials or { })) { } (
        lib.attrValues selectedProviders
      );
    in
    lib.recursiveUpdate profile {
      network = {
        credentials = providers;
        custom_credentials = lib.mapAttrs (_: provider: provider.route) selectedProviders;
      };
      env_credentials = envCredentials;
    };

  piProfile = mkProfile {
    profile = {
      extends = "default";
      meta.name = "pi";
      groups = {
        include = piRuntimeGroups;
        exclude = [ ];
      };
      security = {
        signal_mode = "isolated";
        capability_elevation = false;
      };
      environment.set_vars.JITI_FS_CACHE = "false";
      commands = {
        allow = [ ];
        deny = [ ];
      };
      workdir.access = "readwrite";
      filesystem = {
        allow = [
          "$HOME/.config/pi/agent"
          "$HOME/.config/nono/profile-drafts"
        ];
        read = [
          "$XDG_CONFIG_HOME/clojure"
          "$XDG_CONFIG_HOME/nrepl"
          "$XDG_CONFIG_HOME/clj-kondo"
          "$HOME/vendor/jdks"
          "$HOME/src/git.outskirtslabs.com/casey/skills/skills"
          "$HOME/src/github.com/ramblurr/pi-extensions"
        ];
        write = [ ];
        allow_file = [ ];
        read_file = [ ];
        write_file = [ ];
        deny = [ ];
        bypass_protection = [ ];
        suppress_save_prompt = [ "/etc/nix/sentry-endpoint" ];
      };
      network = {
        block = false;
        allow_domain = [ ];
        open_port = [ ];
        listen_port = [ ];
      };
      hooks = { };
      open_urls = {
        allow_origins = [
          "https://auth.openai.com"
          "https://claude.ai"
          "https://github.com"
        ];
        allow_localhost = true;
      };
      allow_launch_services = true;
      rollback = {
        exclude_patterns = [
          "node_modules"
          ".next"
          "__pycache__"
          "target"
          ".pi"
          ".cpcache"
          ".direnv"
          ".zig-cache"
          ".zig-cache-global"
          "zig-out"
          ".gradle"
          "build"
        ];
        exclude_globs = [ "*.tmp.[0-9]*.[0-9]*" ];
      };
    };
  };

  ecaProfile = mkProfile {
    providers = [
      "openrouter"
      "openai"
      "anthropic"
      "gemini"
      "cerebras"
      "mistral"
    ];
    profile = {
      extends = "default";
      meta.name = "eca";
      groups = {
        include = agentRuntimeGroups;
        exclude = [ ];
      };
      security = {
        signal_mode = "isolated";
        capability_elevation = false;
      };
      workdir.access = "readwrite";
      filesystem = {
        allow = [ "$HOME/.cache/eca" ];
        read = [ "$HOME/.config/eca" ];
      };
      network = {
        block = false;
        allow_domain = [ ];
        open_port = [ ];
        listen_port = [ ];
      };
    };
  };

  ecaProxyConfig = {
    providers.openrouter = {
      api = "openai-chat";
      url = "\${env:OPENROUTER_BASE_URL}";
      key = "\${env:OPENROUTER_API_KEY}";
      models = lib.genAttrs models.openrouter (_: { });
    };
  };
in
{
  imports = [ ./bb.nix ];
  # TODO disable smolvm due to failed upstream pkg build
  #environment.systemPackages = [ inputs.smolvm.packages.${pkgs.stdenv.hostPlatform.system}.default ];

  myhm.home.sessionVariables = {
    VIBE_HOME = "$XDG_CONFIG_HOME/vibe";
  };

  modules.dev.llms.profile = {
    enable = true;
    piProviderCredentials = {
      anthropic = "op://LLM/Anthropic-quine/credential";
      typesafe = "op://LLM/Jev-quine/credential";
      subq = "op://LLM/Subq-quine/credential";
      openai = "op://LLM/OpenAI-quine/credential";
    };
  };
  modules.dev.llms.desktop.enable = true;
  modules.dev.llms.ollama.enable = true;
  modules.dev.llms.onepasswordConnect.enable = true;
  modules.dev.llms.proxy.enable = true;

  modules.dev.llms.cliproxy-api = {
    enable = true;
    tailscaleServePort = 8317;
    settings = {
      server.trusted-proxies = [
        "127.0.0.1"
        "::1"
      ];
      management = {
        allow-remote = true;
        secret-key = "op://LLM/CLIProxyAPI/management-secret-unhashed";
      };
      access.api-keys = [
        "op://LLM/CLIProxyAPI/credential"
        "op://moot/cliproxyapi hirde/credential"
      ];
      api-keys.openai-compatibility = [
        {
          name = "openrouter";
          base-url = "https://openrouter.ai/api/v1";
          keys = [ { api-key = "op://LLM/OpenRouter/credential"; } ];
          models = map (name: { inherit name; }) models.openrouter;
        }
        {
          name = "subq";
          base-url = "https://api.subq.ai/v1";
          keys = [ { api-key = config.modules.dev.llms.profile.piProviderCredentials.subq; } ];
          models = [ { name = models.subq.savant.id; } ];
        }
        {
          name = "mistral";
          base-url = "https://api.mistral.ai/v1";
          keys = [ { api-key = "op://LLM/Mistral-quine/credential"; } ];
          models = [ { name = models.mistral.large4.id; } ];
        }
      ];
      routing = {
        strategy = "fill-first";
        session-affinity = true;
        session-affinity-ttl = "1h";
        session-affinity-rebind-on = "quota";
        retry = {
          request-retry = 3;
          max-retry-interval = 30;
        };
        cooldown.transient-error-cooldown-seconds = 15;
      };
    };
    secretFiles."op://moot/cliproxyapi hirde/credential" =
      "/home/ramblurr/.local/state/cliproxyapi/hirde-key";
  };

  modules.dev.llms.t3 = {
    desktop.enable = true;
    tailscaleServePort = 8443;
  };
  modules.dev.llms.nono = {
    enable = true;
    profiles = {
      pi = piProfile;
      eca = ecaProfile;
    };
    ecaSettings = ecaProxyConfig;
  };
}
