{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  cfg = config.modules.dev.llms;
  jsonFormat = pkgs.formats.json { };
  llmAgents = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
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

  providerCatalog = {
    openrouter.route = {
      upstream = "https://openrouter.ai/api/v1";
      credential_key = "op://Private/OpenRouter/OPENROUTER_API_KEY";
      env_var = "OPENROUTER_API_KEY";
    };

    openai.route = {
      upstream = "https://api.openai.com/v1";
      credential_key = "op://Private/OpenAI API/OPENAI_API_KEY_quine";
      env_var = "OPENAI_API_KEY";
      inject_header = "Authorization";
      credential_format = "Bearer {}";
    };

    anthropic.route = {
      upstream = "https://api.anthropic.com";
      credential_key = "op://Private/Anthropic/api-key-quine";
      env_var = "ANTHROPIC_API_KEY";
      inject_header = "x-api-key";
      credential_format = "{}";
    };

    gemini = {
      route = {
        upstream = "https://generativelanguage.googleapis.com";
        credential_key = "op://Private/Google AI Studio/api-key-quine";
        env_var = "GEMINI_API_KEY";
        inject_header = "x-goog-api-key";
        credential_format = "{}";
      };
      envCredentials = {
        "op://Private/Google AI Studio/GOOGLE_CLOUD_PROJECT" = "GOOGLE_CLOUD_PROJECT";
      };
    };

    cerebras.route = {
      upstream = "https://api.cerebras.ai/v1";
      credential_key = "op://Private/Cerebras/CEREBRAS_API_KEY_quine";
      env_var = "CEREBRAS_API_KEY";
      inject_header = "Authorization";
      credential_format = "Bearer {}";
    };

    mistral.route = {
      upstream = "https://api.mistral.ai/v1";
      credential_key = "op://Private/Mistral/api-key-quine";
      env_var = "MISTRAL_API_KEY";
      inject_header = "Authorization";
      credential_format = "Bearer {}";
    };

    elevenlabs.route = {
      upstream = "https://api.elevenlabs.io";
      credential_key = "op://Private/ElevenLabs/api-key-quine";
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
        include = agentRuntimeGroups;
        exclude = [ ];
      };
      security = {
        signal_mode = "isolated";
        capability_elevation = false;
      };
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
          "$HOME/src/github.com/ramblurr/nix-devenv/skills"
          "$HOME/src/github.com/ramblurr/pi-extensions"
        ];
        write = [ ];
        allow_file = [ ];
        read_file = [ ];
        write_file = [ ];
        deny = [ ];
        bypass_protection = [ ];
        suppress_save_prompt = [ ];
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

  nono-eca = pkgs.writeShellScriptBin "nono-eca" ''
    exec ${lib.getExe pkgs.nono} --silent run \
      --profile eca \
      --allow-cwd \
      -- ${lib.getExe llmAgents.eca} "$@"
  '';
in
{
  config = lib.mkIf cfg.enable {
    myhm = {
      xdg.configFile."nono/profiles/pi.json".source =
        jsonFormat.generate "nono-profile-pi.json" piProfile;
      xdg.configFile."nono/profiles/eca.json".source =
        jsonFormat.generate "nono-profile-eca.json" ecaProfile;
      home.packages = [ nono-eca ];
    };
  };
}
