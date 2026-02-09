{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  cfg = config.modules.dev.llms;
  llm-agents = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
  inherit (config.modules.users.primaryUser) username;
  inherit (llm-agents)
    opencode
    pi
    mistral-vibe
    gemini-cli
    catnip
    ;
  wrapWithLLMKeys = cmd: removeVars: ''
    #!${pkgs.runtimeShell}
    export PI_CONFIG_DIR="$HOME/.config/pi"
    export VIBE_HOME="$HOME/.config/vibe"
    if [ -f "$HOME/.llm-keys" ]; then
      set -a  # automatically export all variables
      source "$HOME/.llm-keys"
      set +a  # turn off automatic export
      ${lib.concatStringsSep "\n" (map (v: "unset ${v}") removeVars)}
    fi
    exec ${cmd} "$@"
  '';
  opencode-wrapper = pkgs.writeShellScriptBin "opencode" (
    wrapWithLLMKeys "${opencode}/bin/opencode" [
      "ANTHROPIC_API_KEY"
    ]
  );
  llmWithPlugins = pkgs.llm.withPlugins {
    llm-cmd = true;
    llm-anthropic = true;
    llm-docs = true;
    llm-fragments-reader = true;
    llm-gemini = true;
    llm-git = true;
    llm-jq = true;
    llm-hacker-news = true;
    llm-mistral = true;
    llm-ollama = true;
    llm-openai-plugin = true;
    llm-pdf-to-images = true;
  };
  llm-wrapper = pkgs.writeShellScriptBin "llm" (wrapWithLLMKeys "${llmWithPlugins}/bin/llm" [ ]);
  gemini-cli-wrapper = pkgs.writeShellScriptBin "gemini" (
    wrapWithLLMKeys "${gemini-cli}/bin/gemini" [ ]
  );
  github-mcp-server-wrapper = pkgs.writeShellScriptBin "github-mcp-server" (
    wrapWithLLMKeys "${pkgs.github-mcp-server}/bin/github-mcp-server" [ ]
  );
  catnip-wrapper = pkgs.writeShellScriptBin "catnip" (wrapWithLLMKeys "${catnip}/bin/catnip" [ ]);
  pi-wrapper = pkgs.writeShellScriptBin "pi" (wrapWithLLMKeys "${pi}/bin/pi" [ ]);
  mistral-vibe-wrapper = pkgs.writeShellScriptBin "vibe" (
    wrapWithLLMKeys "${mistral-vibe}/bin/vibe" [ ]
  );
  whisper-cpp =
    if cfg.cudaSupport then (pkgs.whisper-cpp.override { cudaSupport = true; }) else pkgs.whisper-cpp;
  cat-url-markdown = pkgs.writeShellScriptBin "cat-url-markdown" ''
    if [ -z "$1" ]; then
      echo "usage: $(basename "$0") URL [FILENAME]"
      exit 1
    fi
    curl -sSL --output - $(printf "https://r.jina.ai/%s" $1)
  '';
in
{
  options.modules.dev.llms = {
    enable = lib.mkEnableOption "";
    cudaSupport = lib.mkEnableOption {
      description = "Enable CUDA support";
    };
  };
  config = lib.mkIf cfg.enable {
    environment.persistence."/persist" = {
      directories = [ ];
      users.${username} = {
        directories = [
          ".config/claude"
          ".config/pi"
          ".config/vibe"
          ".config/codex"
          ".config/crush"
          ".config/eca"
          ".config/opencode"
          ".local/share/crush"
          ".local/share/opencode"
          ".local/state/opencode"
        ];
      };
    };
    services.ollama = {
      enable = true;
      package = pkgs.ollama-cuda;
    };
    services.open-webui.enable = true;
    myhm = {
      sops.secrets.llm-keys = {
        mode = "0400";
        path = ".llm-keys";
      };

      home.sessionVariables = {
        PLAYWRIGHT_BROWSERS_PATH = "${pkgs.playwright.browsers}";
      };
      home.packages = with pkgs; [
        playwright
        playwright-test
        playwright-mcp
        piper-tts
        espeak
        jujutsu
        mcp-inspector
        llm-wrapper
        github-mcp-server-wrapper
        gemini-cli-wrapper
        catnip-wrapper
        pi-wrapper
        mistral-vibe-wrapper
        #codex
        #inputs.boxai.packages.${pkgs.stdenv.hostPlatform.system}.boxai
        opencode-wrapper
        cat-url-markdown
        whisper-cpp
        inputs.tmux-buddy.packages.${pkgs.stdenv.hostPlatform.system}.default
        ollama-cuda
        dotool # handy (speech to text) uses this for clipboard access
        wtype # handy (speech to text) uses this for clipboard access
        llm-agents.claude-code
        llm-agents.codex
        llm-agents.ccusage
        llm-agents.handy
      ];
    };
  };
}
