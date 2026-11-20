{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.dev.llms;
  models = import ./models.nix;
  packages = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
  agentConfigHome = "${config.modules.users.primaryUser.homeDirectory}/.config";
  opRead =
    if cfg.onepasswordConnect.tokenFile == null then
      "${lib.getExe pkgs._1password-cli} read"
    else
      lib.getExe cfg.onepasswordConnect.package;
  cliproxyProvider = import ./cliproxy-pi-provider.nix {
    inherit (cfg.proxy) url;
    extraModels = cfg.profile.piProxyExtraModels;
    apiKey = "!${opRead} ${lib.escapeShellArg cfg.proxy.credential}";
  };
  piProviders =
    lib.mapAttrs (_: credential: {
      apiKey = "!${opRead} ${lib.escapeShellArg credential}";
    }) cfg.profile.piProviderCredentials
    // lib.optionalAttrs cfg.proxy.enable { cliproxy = cliproxyProvider; };
  piInstance = piPackages: {
    settings = {
      defaultProvider = if cfg.proxy.enable then "cliproxy" else "openai-codex";
      defaultModel = models.defaults.pi;
      defaultThinkingLevel = "high";
      skills = lib.optionals cfg.skills.enable (
        map (name: "${cfg.skills.package.plugins.${name}}/skills") [
          "my"
          "tstack"
        ]
      );
      packages = map toString piPackages;
      npmCommand = [ "pnpm" ];
      hideThinkingBlock = true;
      theme = "dark";
      steeringMode = "all";
      showCacheMissNotices = true;
      shellCommandPrefix = "eval \"$(DEVSHELL_NO_MOTD=1 direnv export bash 2>/dev/null)\"";
    }
    // lib.optionalAttrs (cfg.profile.piProviderCredentials ? subq) {
      defaultTools = [ "+codemode" ];
    };
    models.providers = piProviders;
    fff = {
      mode = "override";
      enableHomeDirScanning = false;
      enableFsRootScanning = false;
    };
  };
  codexSettings = {
    model = models.defaults.codex;
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
      env_key = "CLIPROXY_API_KEY";
    };
  };
  claudeSettings = {
    cleanupPeriodDays = 99999;
    includeCoAuthoredBy = false;
    model = models.defaults.claude;
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
        enabled = cfg.pi.enable;
      };
      claudeAgent = {
        driver = "claudeAgent";
        enabled = cfg.claude.enable;
        config.homePath = "${agentConfigHome}/claude";
      };
      codex = {
        driver = "codex";
        enabled = cfg.codex.enable;
        config.homePath = "${agentConfigHome}/codex";
      };
      antigravity = {
        driver = "antigravity";
        enabled = true;
        config.binaryPath = lib.getExe packages.antigravity-cli;
      };
    };
  };
in
{
  options.modules.dev.llms.profile = {
    enable = lib.mkEnableOption "the shared Codex, Claude, Pi, and T3 Code client setup with harness-plugins skills";
    piProviderCredentials = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      example = {
        anthropic = "op://LLM/Anthropic-example/credential";
      };
      description = "1Password references for Pi's direct providers, keyed by Pi provider name. The proxy provider is added when `proxy.enable` is set.";
    };
    piProxyExtraModels = lib.mkOption {
      type = lib.types.listOf lib.types.attrs;
      default = [ ];
      description = "Additional Pi model records for routes supported by the configured proxy.";
    };
    codexSessionHookUsers = lib.mkOption {
      type = lib.types.nullOr (lib.types.listOf lib.types.str);
      default = null;
      example = [ "alice" ];
      description = "Users whose Codex sessions run the pstack session hook from /etc/codex/config.toml, null for every user, or [ ] for none. Codex reads that file for all accounts.";
    };
  };

  config = lib.mkIf cfg.profile.enable {
    myhm = {
      home.packages = [ packages.antigravity-cli ];
      home.sessionVariables = {
        HINDSIGHT_CONFIG_DIR = "$XDG_CONFIG_HOME/hindsight";
        PI_CODING_AGENT_DIR = "$XDG_CONFIG_HOME/pi/agent";
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
    modules.dev.llms = {
      enable = true;
      agents.extraFiles = [ ./plannotator-instructions.md ];
      skills.enable = true;
      t3 = {
        enable = true;
        settings = t3Settings;
      };
      pi = {
        enable = true;
        package = packages.pi.override { useBun = false; };
        instances = {
          pi = piInstance [
            pkgs.pi-reload
            cfg.skills.package.plugins.pstack
            pkgs.pi-speak
            pkgs.pi-subq
            pkgs.brepl-balance
            pkgs.plannotator-pi-extension
            pkgs.pi-openai-fast
            pkgs.pi-link
            pkgs.pi-lens
            pkgs.pi-fff
          ];
          pi-t3 = piInstance [
            pkgs.pi-reload
            cfg.skills.package.plugins.pstack
            pkgs.pi-speak
            pkgs.pi-subq
            pkgs.brepl-balance
            pkgs.pi-openai-fast
            pkgs.pi-lens
            pkgs.pi-fff
          ];
        };
      };
      codex = {
        enable = true;
        marketplaces = lib.optional cfg.skills.enable cfg.skills.package;
        plugins = lib.optionals cfg.skills.enable [
          "my@harness-plugins"
          "pstack@harness-plugins"
          "tstack@harness-plugins"
        ];
        settings = codexSettings;
        removeSettings = [
          [
            "features"
            "skills"
          ]
        ]
        ++ [
          (
            if cfg.proxy.enable then
              [
                "model_providers"
                "cliproxy"
                "experimental_bearer_token"
              ]
            else
              [
                "model_providers"
                "cliproxy"
              ]
          )
        ];
      };
      claude = {
        enable = true;
        plugins = lib.optionalAttrs cfg.skills.enable {
          inherit (cfg.skills.package.plugins)
            my
            pstack
            tstack
            ;
        };
        settings = claudeSettings;
        removeSettings = [
          [
            "env"
            "ANTHROPIC_AUTH_TOKEN"
          ]
          [
            "env"
            (if cfg.proxy.enable then "ANTHROPIC_API_KEY" else "ANTHROPIC_BASE_URL")
          ]
        ];
      };
    };

    environment.etc."codex/config.toml" =
      lib.mkIf (cfg.skills.enable && cfg.profile.codexSessionHookUsers != [ ])
        {
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
                      command =
                        lib.optionalString (cfg.profile.codexSessionHookUsers != null) ''
                          case "$(id -un)" in ${
                            lib.concatMapStringsSep "|" lib.escapeShellArg cfg.profile.codexSessionHookUsers
                          }) ;; *) exit 0 ;; esac;
                        ''
                        + "CLAUDE_PLUGIN_ROOT=${pstack} ${pstack}/hooks/session-start.sh codex";
                    }
                  ];
                }
              ];
            };
        };
  };
}
