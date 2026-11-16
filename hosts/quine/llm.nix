_: {
  imports = [ ./bb.nix ];
  # TODO disable smolvm due to failed upstream pkg build
  #environment.systemPackages = [ inputs.smolvm.packages.${pkgs.stdenv.hostPlatform.system}.default ];

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
          models = [ { name = "moonshotai/kimi-k2"; } ];
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
    enable = true;
    desktop.enable = true;
    tailscaleServePort = 8443;
  };
}
