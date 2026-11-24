_: {
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

  modules.dev.llms.t3 = {
    desktop.enable = true;
    tailscaleServePort = 8443;
  };
}
