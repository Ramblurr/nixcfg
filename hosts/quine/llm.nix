{
  config,
  inputs,
  pkgs,
  ...
}:
let
  models = import ../../modules/dev/llms/models.nix;
in
{
  # imports = [ ./bb.nix ];
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
    enable = false;
    package = import ../../pkgs/cli-proxy-api.nix {
      inherit (inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}) cli-proxy-api;
    };
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
        strategy = "soonest-reset";
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
  };

  modules.dev.llms.t3 = {
    desktop.enable = true;
    tailscaleServePort = 8443;
  };
}
