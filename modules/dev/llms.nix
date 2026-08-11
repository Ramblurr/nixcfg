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
    llm-ollama = cfg.ollama.enable;
    llm-openai-plugin = true;
    llm-pdf-to-images = true;
  };
  paseo-cli = inputs.paseo.packages.${pkgs.stdenv.hostPlatform.system}.default;
  paseo-wrapper = pkgs.writeShellScriptBin "paseo" ''
    export PASEO_PASSWORD="$(<${cfg.paseo.passwordFile})"
    exec ${paseo-cli}/bin/paseo "$@"
  '';
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
    ollama.enable = lib.mkEnableOption "";
    paseo.passwordFile = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "File containing the Paseo CLI password.";
    };
  };
  config = lib.mkIf cfg.enable {
    services.ollama = lib.mkIf cfg.ollama.enable {
      enable = true;
      package = pkgs.ollama-cuda;
    };
    services.open-webui.enable = cfg.ollama.enable;
    services.open-webui.port = 11180;
    myhm = {
      home.sessionVariables = {
        #PLAYWRIGHT_BROWSERS_PATH = "${pkgs.playwright.browsers}";
        CLAUDE_CONFIG_DIR = "$XDG_CONFIG_HOME/claude";
        CODEX_HOME = "$XDG_CONFIG_HOME/codex";
        HINDSIGHT_CONFIG_DIR = "$XDG_CONFIG_HOME/hindsight";
        PI_CODING_AGENT_DIR = "$XDG_CONFIG_HOME/pi/agent";
        VIBE_HOME = "$XDG_CONFIG_HOME/vibe";
        PLANNOTATOR_DATA_DIR = "$XDG_CONFIG_HOME/plannotator";
        PLANNOTATOR_GLIMPSE = "0";
        PLANNOTATOR_SHARE = "disabled";
        PASEO_HOME = "$XDG_STATE_HOME/paseo";
      };
      home.packages =
        with pkgs;
        [
          #playwright
          #playwright-test
          #playwright-mcp
          geckodriver
          chromedriver
          glimpseui
          #dirge
          piper-tts
          espeak
          jujutsu
          #mcp-inspector
          llmWithPlugins
          pkgs.github-mcp-server
          llm-agents.pi
          inputs.paseo.packages.${pkgs.stdenv.hostPlatform.system}.desktop
          #llm-agents.vix
          #llm-agents.mistral-vibe
          #codex
          #inputs.boxai.packages.${pkgs.stdenv.hostPlatform.system}.boxai
          cat-url-markdown
          inputs.tmux-buddy.packages.${pkgs.stdenv.hostPlatform.system}.default
          dotool # handy (speech to text) uses this for clipboard access
          wtype # handy (speech to text) uses this for clipboard access
          llm-agents.annot
          llm-agents.claude-code
          #llm-agents.code-review-graph
          llm-agents.codex
          #llm-agents.jscpd
          llm-agents.plannotator
          pkgs.hindsight-cli
          #ccusage
          inputs.git-lines.packages.${pkgs.stdenv.hostPlatform.system}.default
          universal-ctags
          difftastic
          ast-grep
          nushell
          pkgs.nono
        ]
        ++ lib.optionals (cfg.paseo.passwordFile != null) [
          paseo-wrapper
        ]
        ++ lib.optionals cfg.ollama.enable [
          ollama-cuda
        ];
    };
  };
}
