{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.dev.llms;
  packages = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
  agentConfigHome = "${config.modules.users.primaryUser.homeDirectory}/.config";
  connect = cfg.onepasswordConnect;
  opRead =
    if connect.tokenFile == null then
      "${lib.getExe pkgs._1password-cli} read"
    else
      lib.getExe connect.package;
  cliproxyRoot = cfg.proxy.url;
  # Static so they exist the moment Pi starts
  cliproxyOpenaiModel = id: name: offLevel: {
    inherit id name;
    api = "openai-responses";
    baseUrl = "${cliproxyRoot}/v1";
    reasoning = true;
    input = [
      "text"
      "image"
    ];
    contextWindow = 272000;
    maxTokens = 128000;
    # Billed to the subscriptions behind the proxy, not per token.
    cost = {
      input = 0;
      output = 0;
      cacheRead = 0;
      cacheWrite = 0;
    };
    thinkingLevelMap = {
      off = offLevel;
      minimal = null;
      low = "low";
      medium = "medium";
      high = "high";
      xhigh = "xhigh";
      max = "max";
    };
    compat = {
      supportsStrictMode = true;
      supportsOpenAIGrammarTools = true;
      supportsAdditionalTools = true;
      supportsToolSearch = true;
      supportsMidConvoSystemMessages = true;
      supportsExplicitPromptCacheMode = true;
      supportsMidConvoEffort = true;
    };
  };
  cliproxyMuseModel = id: name: {
    inherit id name;
    api = "openai-responses";
    baseUrl = "${cliproxyRoot}/v1";
    reasoning = true;
    input = [
      "text"
      "image"
    ];
    contextWindow = 1048576;
    maxTokens = 131072;
    cost = {
      input = 0;
      output = 0;
      cacheRead = 0;
      cacheWrite = 0;
    };
    thinkingLevelMap = {
      off = null;
      minimal = "minimal";
      low = "low";
      medium = "medium";
      high = "high";
      xhigh = "xhigh";
      max = "max";
    };
    inputLimits.images.resize = {
      maxWidth = 2000;
      maxHeight = 2000;
      maxBytes = 4718592;
      jpegQuality = 80;
    };
  };
  cliproxyProvider = {
    baseUrl = cliproxyRoot;
    apiKey = "!${opRead} ${lib.escapeShellArg cfg.proxy.credential}";
    models = [
      (cliproxyOpenaiModel "gpt-6.2-sol" "GPT-6.2 Sol" null)
      (cliproxyOpenaiModel "gpt-6.1-terra" "GPT-6.1 Terra" "none")
      (cliproxyOpenaiModel "gpt-6.2-astra" "GPT-6.2 Astra" null)
      (cliproxyOpenaiModel "gpt-6-luna" "GPT-6 Luna" "none")
      (cliproxyMuseModel "muse-wonder-1.0" "Muse Wonder")
      (cliproxyMuseModel "muse-wonder-1.0-contributor" "Muse Wonder (Contributor)")
      (cliproxyMuseModel "muse-wonder-full-1.0" "Muse Wonder Full")
      (cliproxyMuseModel "muse-wonder-full-1.0-contributor" "Muse Wonder Full (Contributor)")
    ];
  };
  all-providers = {
    anthropic.apiKey = "!${opRead} 'op://LLM/Anthropic-quine/credential'";
    typesafe.apiKey = "!${opRead} 'op://LLM/Jev-quine/credential'";
    subq.apiKey = "!${opRead} 'op://LLM/Subq-quine/credential'";
    openai.apiKey = "!${opRead} 'op://LLM/OpenAI-quine/credential'";
  }
  // lib.optionalAttrs cfg.proxy.enable { cliproxy = cliproxyProvider; };
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
      models = lib.genAttrs [
        "google/gemini-4-carbon"
        "z-ai/glm-5.3-flash"
        "deepseek/deepseek-v4.1-flash-1017"
        "moonshotai/kimi-k3.1-pro"
        "tencent/hy4"
      ] (_: { });
    };
  };
  codexSettings = {
    model = "gpt-6-luna";
    model_reasoning_effort = "medium";
    web_search = "disabled";
    notice.hide_rate_limit_model_nudge = true;
    plugins."github@openai-curated".enabled = true;
    # pstack's session-start hook runs from /etc/codex/config.toml, where Codex trusts it.
    hooks.state."pstack@harness-plugins:hooks/codex-hooks.json:session_start:0:0".enabled = false;
    mcp_servers.plannotator-inbox = {
      command = lib.getExe' packages.plannotator "plannotator";
      args = [
        "inbox"
        "mcp"
      ];
    };
    service_tier = "fast";
    approval_policy = "never";
    sandbox_mode = "danger-full-access";
    model_provider = if cfg.proxy.enable then "cliproxy" else "openai";
    features = {
      hooks = true;
    };
    tui.notifications = false;
  }
  // lib.optionalAttrs cfg.proxy.enable {
    model_providers.cliproxy = {
      name = "CLIProxyAPI";
      base_url = "${cfg.proxy.url}/v1";
      wire_api = "responses";
      requires_openai_auth = false;
      supports_websockets = true;
    }
    // lib.optionalAttrs cfg.proxy.removeLegacyCredentials { env_key = "CLIPROXY_API_KEY"; };
  };
  claudeSettings = {
    cleanupPeriodDays = 99999;
    includeCoAuthoredBy = false;
    model = "opus";
    spinnerTipsEnabled = false;
    alwaysThinkingEnabled = true;
    effortLevel = "xhigh";
    modelSettings."claude-opus-5".effortLevel = "high";
    tui = "fullscreen";
    editorMode = "vim";
    preferredNotifChannel = "terminal_bell";
    terminalProgressBarEnabled = false;
    env.ENABLE_CLAUDEAI_MCP_SERVERS = "false";
  }
  // lib.optionalAttrs cfg.proxy.enable {
    env = {
      ENABLE_CLAUDEAI_MCP_SERVERS = "false";
      ANTHROPIC_BASE_URL = cfg.proxy.url;
    };
  };
  t3Settings = {
    enableProviderUpdateChecks = false;
    storageCleanup = {
      worktreeAfterDays = 90;
      worktreeOnMerge = true;
    };
    projectSettingsFolded = true;
    idleCompaction = "onReturn";
    providerInstances = {
      pi = {
        driver = "pi";
        displayName = "Pi";
        environment = [
          {
            name = "PI_CODING_AGENT_DIR";
            value = "${agentConfigHome}/pi-t3/agent";
            sensitive = false;
          }
        ];
        enabled = config.modules.dev.llms.pi.enable;
      };
      claudeAgent = {
        driver = "claudeAgent";
        enabled = config.modules.dev.llms.claude.enable;
        config.homePath = "${agentConfigHome}/claude";
      };
      codex = {
        driver = "codex";
        enabled = config.modules.dev.llms.codex.enable;
        config.homePath = "${agentConfigHome}/codex";
      };
    };
  };
in
{
  imports = [ ./bb.nix ];
  # TODO disable smolvm due to failed upstream pkg build
  #environment.systemPackages = [ inputs.smolvm.packages.${pkgs.stdenv.hostPlatform.system}.default ];

  myhm = {
    home.sessionVariables = {
      #PLAYWRIGHT_BROWSERS_PATH = "${pkgs.playwright.browsers}";
      HINDSIGHT_CONFIG_DIR = "$XDG_CONFIG_HOME/hindsight";
      PI_CODING_AGENT_DIR = "$XDG_CONFIG_HOME/pi/agent";
      VIBE_HOME = "$XDG_CONFIG_HOME/vibe";
      PLANNOTATOR_DATA_DIR = "$XDG_CONFIG_HOME/plannotator";
      PLANNOTATOR_GLIMPSE = "0";
      PLANNOTATOR_SHARE = "disabled";
      UCP_HOME = "$XDG_STATE_HOME/ucp";
      SPEL_CONFIG = "$XDG_CONFIG_HOME/spel/config.json";
      SPEL_PROFILE = "$XDG_STATE_HOME/spel/profile";
      PI_LENS_DISABLE_LSP_INSTALL = "1";
      PI_LENS_DISABLE_TOOL_INSTALL = "1";
    };
    xdg.configFile."spel/config.json".text = builtins.toJSON {
      contentBoundaries = true;
    };
  };

  modules.dev.llms.enable = true;
  modules.dev.llms.agents.extraFiles = [ ../../modules/dev/llms/plannotator-instructions.md ];
  modules.dev.llms.ollama.enable = true;
  modules.dev.llms.onepasswordConnect.tokenFile =
    config.sops.secrets.onepassword-connect-llm-token.path;
  modules.dev.llms.proxy = {
    enable = true;
    url = "https://quine.${config.repo.secrets.global.domain.tailnet}:8317";
    credential = "op://LLM/CLIProxyAPI/credential";
  };
  modules.dev.llms.skills = {
    enable = true;
    package = pkgs.harness-plugins;
  };

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
          models = map (name: { inherit name; }) [
            "google/gemini-4-carbon"
            "z-ai/glm-5.3-flash"
            "deepseek/deepseek-v4.1-flash-1017"
            "moonshotai/kimi-k3.1-pro"
            "tencent/hy4"
          ];
        }
      ];
      routing = {
        strategy = "soonest-reset";
        session-affinity = true;
        session-affinity-ttl = "1h";
        session-affinity-rebind-on = "quota";
        retry = {
          request-retry = 3;
          max-retry-interval = 30;
        };
      };
    };
    secretFiles."op://moot/cliproxyapi hirde/credential" =
      "/home/ramblurr/.local/state/cliproxyapi/hirde-key";
  };

  modules.dev.llms.t3 = {
    settings = t3Settings;
    enable = true;
    desktop.enable = true;
    tailscaleServePort = 8443;
  };
  modules.dev.llms.pi = {
    enable = true;
    # pi-hashline-edit-pro imports node:sqlite, unavailable in Bun's standalone runtime.
    package = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.pi.override {
      useBun = false;
    };
    instances = {
      pi = {
        settings = {
          defaultProvider = if cfg.proxy.enable then "cliproxy" else "openai-codex";
          defaultModel = "gpt-6.2-sol";
          defaultThinkingLevel = "high";
          skills = lib.optionals cfg.skills.enable (
            map (name: "${cfg.skills.package.plugins.${name}}/skills") [
              "my"
              "tstack"
            ]
          );
          packages = map toString [
            pkgs.pi-reload
            cfg.skills.package.plugins.pstack
            pkgs.pi-speak
            #pkgs.pi-heartbeat
            #pkgs.pi-ghost
            "${pkgs.pi-cliproxy.src}/pi-subq"
            pkgs.brepl-balance
            #pkgs.pi-sexp-edit
            #pkgs.pi-nrepl
            #pkgs.pi-hashline-edit-pro
            pkgs.plannotator-pi-extension
            #pkgs.epimetheus
            #pkgs.pi-mcp-adapter
            pkgs.pi-openai-fast
            pkgs.pi-cliproxy
            #pkgs.pi-nono-sandbox
            #pkgs.pi-matrix-relay
            pkgs.pi-link
            pkgs.pi-lens
            pkgs.pi-fff
            #pkgs.pi-goal-x
          ];
          npmCommand = [ "pnpm" ];
          hideThinkingBlock = true;
          theme = "dark";
          steeringMode = "all";
          showCacheMissNotices = true;
          shellCommandPrefix = "eval \"$(DEVSHELL_NO_MOTD=1 direnv export bash 2>/dev/null)\"";
        };
        models.providers = all-providers;
        fff = {
          mode = "override";
          enableHomeDirScanning = false;
          enableFsRootScanning = false;
        };
      };
      pi-t3 = {
        settings = {
          defaultProvider = if cfg.proxy.enable then "cliproxy" else "openai-codex";
          defaultModel = "gpt-6.2-sol";
          defaultThinkingLevel = "high";
          skills = lib.optionals cfg.skills.enable (
            map (name: "${cfg.skills.package.plugins.${name}}/skills") [
              "my"
              "tstack"
            ]
          );
          packages = map toString [
            pkgs.pi-reload
            cfg.skills.package.plugins.pstack
            pkgs.pi-speak
            "${pkgs.pi-cliproxy.src}/pi-subq"
            pkgs.brepl-balance
            pkgs.pi-openai-fast
            pkgs.pi-cliproxy
            pkgs.pi-lens
            pkgs.pi-fff
          ];
          npmCommand = [ "pnpm" ];
          hideThinkingBlock = true;
          theme = "dark";
          steeringMode = "all";
          showCacheMissNotices = true;
          shellCommandPrefix = "eval \"$(DEVSHELL_NO_MOTD=1 direnv export bash 2>/dev/null)\"";
        };
        models.providers = all-providers;
        fff = {
          mode = "override";
          enableHomeDirScanning = false;
          enableFsRootScanning = false;
        };
      };
    };
  };
  environment.etc."codex/config.toml" = lib.mkIf cfg.skills.enable {
    source =
      let
        pstack = cfg.skills.package.plugins.pstack;
      in
      (pkgs.formats.toml { }).generate "codex-system-config.toml" {
        hooks.SessionStart = [
          {
            matcher = "startup|resume|clear|compact";
            hooks = [
              {
                type = "command";
                command = "CLAUDE_PLUGIN_ROOT=${pstack} ${pstack}/hooks/session-start.sh codex";
              }
            ];
          }
        ];
      };
  };

  modules.dev.llms.codex = {
    enable = true;
    marketplaces = lib.optional cfg.skills.enable cfg.skills.package;
    plugins = lib.optionals cfg.skills.enable [
      "my@harness-plugins"
      "pstack@harness-plugins"
      "tstack@harness-plugins"
    ];
    extraArgs = lib.optionals cfg.proxy.enable [
      "-c"
      "model_providers.cliproxy.env_key=\"CLIPROXY_API_KEY\""
    ];
    settings = codexSettings;
    removeSettings = [
      [
        "features"
        "skills"
      ]
    ]
    ++ lib.optionals (!cfg.proxy.enable) [
      [
        "model_providers"
        "cliproxy"
      ]
    ]
    ++ lib.optionals (cfg.proxy.enable && !cfg.proxy.removeLegacyCredentials) [
      [
        "model_providers"
        "cliproxy"
        "env_key"
      ]
    ]
    ++ lib.optionals (cfg.proxy.enable && cfg.proxy.removeLegacyCredentials) [
      [
        "model_providers"
        "cliproxy"
        "experimental_bearer_token"
      ]
    ];
  };
  modules.dev.llms.claude = {
    enable = true;
    plugins = lib.optionalAttrs cfg.skills.enable {
      inherit (cfg.skills.package.plugins)
        my
        pstack
        tstack
        ;
    };
    settings = claudeSettings;
    removeSettings =
      lib.optionals (!cfg.proxy.enable) [
        [
          "env"
          "ANTHROPIC_AUTH_TOKEN"
        ]
        [
          "env"
          "ANTHROPIC_BASE_URL"
        ]
      ]
      ++ lib.optionals (cfg.proxy.enable && cfg.proxy.removeLegacyCredentials) [
        [
          "env"
          "ANTHROPIC_AUTH_TOKEN"
        ]
        [
          "env"
          "ANTHROPIC_API_KEY"
        ]
      ];
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
