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
  cat-url-markdown = pkgs.writeShellScriptBin "cat-url-markdown" ''
    if [ -z "$1" ]; then
      echo "usage: $(basename "$0") URL [FILENAME]"
      exit 1
    fi
    curl -sSL --output - $(printf "https://r.jina.ai/%s" $1)
  '';
  cat-url-markdown-auto = pkgs.writeShellApplication {
    name = "cat-url-markdown-auto";
    runtimeInputs = [
      pkgs.babashka
      cat-url-markdown
      pkgs.curl
    ];
    text = ''
      exec bb ${../../scripts/cat_url_markdown_auto.clj} "$@"
    '';
  };
in
{
  imports = [
    ./llms/agents.nix
    ./llms/clients.nix
    ./llms/cliproxy-api.nix
    ./llms/nono.nix
    ./llms/pi.nix
    ./llms/profile.nix
    ./llms/t3.nix
  ];
  options.modules.dev.llms = {
    enable = lib.mkEnableOption "";
    ollama.enable = lib.mkEnableOption "";
    desktop.enable = lib.mkEnableOption "graphical agent tools (OpenCode Desktop, Glimpse, and the dotool and wtype typing helpers)";
    onepasswordConnect = {
      enable = lib.mkEnableOption "agent key reads through 1Password Connect, using the onepassword-connect-llm-token SOPS secret";
      tokenFile = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default =
          if cfg.onepasswordConnect.enable then
            config.sops.secrets.onepassword-connect-llm-token.path
          else
            null;
        defaultText = lib.literalExpression "config.sops.secrets.onepassword-connect-llm-token.path when enable is set, else null";
        description = ''
          Runtime path to a 1Password Connect token that can read the LLM vault.
          When set, agent API-key reads use Connect instead of the desktop-approved `op` session.
        '';
      };
      host = lib.mkOption {
        type = lib.types.str;
        default = "http://${builtins.head config.site.net.mgmt.hosts4.onepassword-connect}:8080";
        description = "1Password Connect endpoint used with `tokenFile`.";
      };
      package = lib.mkOption {
        type = lib.types.package;
        readOnly = true;
        default = pkgs.writeShellScriptBin "op-read-llm" ''
          OP_CONNECT_HOST=${lib.escapeShellArg cfg.onepasswordConnect.host} \
          OP_CONNECT_TOKEN="$(< ${lib.escapeShellArg cfg.onepasswordConnect.tokenFile})" \
            exec ${lib.getExe pkgs._1password-cli} read "$@"
        '';
        defaultText = lib.literalMD "`op-read-llm`, built from `host` and `tokenFile`";
        description = "`op-read-llm`, which runs `op read` through Connect with `tokenFile`.";
      };
    };
  };
  config = lib.mkIf cfg.enable {
    sops.secrets.onepassword-connect-llm-token = lib.mkIf cfg.onepasswordConnect.enable {
      owner = config.modules.users.primaryUser.username;
      mode = "0400";
    };
    services.ollama = lib.mkIf cfg.ollama.enable {
      enable = true;
      package = pkgs.ollama-cuda;
    };
    # TODO: Re-enable when ChromaDB's PostHog dependency is fixed: https://github.com/NixOS/nixpkgs/pull/578265
    services.open-webui.enable = false;
    services.open-webui.port = 11180;
    myhm = {
      home.packages =
        with pkgs;
        [
          llm-agents.swamp
          #playwright
          #playwright-test
          #playwright-mcp
          geckodriver
          chromedriver
          spel
          #dirge
          #piper-tts
          #espeak
          #jujutsu
          mcp-inspector
          llmWithPlugins
          llm-agents.opencode
          llm-agents.opencode2
          #llm-agents.vix
          #llm-agents.mistral-vibe
          #codex
          #inputs.boxai.packages.${pkgs.stdenv.hostPlatform.system}.boxai
          cat-url-markdown
          cat-url-markdown-auto
          inputs.tmux-buddy.packages.${pkgs.stdenv.hostPlatform.system}.default
          #llm-agents.annot
          llm-agents.herdr
          llm-agents.collie
          #llm-agents.code-review-graph
          #llm-agents.jscpd
          llm-agents.plannotator
          llm-agents.plannotator-tui
          pkgs.hindsight-cli
          pkgs.ucp-cli
          pkgs.link-cli
          #ccusage
          inputs.git-lines.packages.${pkgs.stdenv.hostPlatform.system}.default
          universal-ctags
          difftastic
          ast-grep
          nushell
        ]
        ++ lib.optionals cfg.desktop.enable [
          pkgs.glimpseui
          llm-agents.opencode-desktop
          pkgs.dotool
          pkgs.wtype
        ]
        ++ lib.optionals cfg.ollama.enable [
          ollama-cuda
        ];
    };
  };
}
