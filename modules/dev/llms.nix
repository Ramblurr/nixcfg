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
  dirge = inputs.dirge.packages.${pkgs.stdenv.hostPlatform.system}.default;
  inherit (llm-agents)
    ccusage
    pi
    mistral-vibe
    vix
    ;
  onnxruntime =
    if cfg.cudaSupport then pkgs.onnxruntime.override { cudaSupport = true; } else pkgs.onnxruntime;
  # llm-agents packages only Voxtype's default Whisper backend. Match
  # upstream's ONNX package for Parakeet and Cohere support, enabling CUDA
  # for Parakeet on hosts where modules.dev.llms.cudaSupport is set.
  voxtype = llm-agents.voxtype.overrideAttrs (oldAttrs: {
    cargoBuildFeatures =
      (oldAttrs.cargoBuildFeatures or [ ])
      ++ [
        "parakeet-load-dynamic"
        "cohere"
      ]
      ++ lib.optionals cfg.cudaSupport [
        "parakeet-cuda"
      ];
    nativeBuildInputs =
      (oldAttrs.nativeBuildInputs or [ ])
      ++ lib.optionals cfg.cudaSupport [
        pkgs.cudaPackages.cuda_nvcc
      ];
    buildInputs =
      (oldAttrs.buildInputs or [ ])
      ++ [ onnxruntime ]
      ++ lib.optionals cfg.cudaSupport [
        pkgs.cudaPackages.cudatoolkit
        pkgs.cudaPackages.cudnn
      ];
    env = (oldAttrs.env or { }) // {
      ORT_LIB_LOCATION = "${onnxruntime}/lib";
    };
    postFixup = (oldAttrs.postFixup or "") + ''
      wrapProgram $out/bin/voxtype \
        --set ORT_DYLIB_PATH "${onnxruntime}/lib/libonnxruntime.so" \
        --prefix LD_LIBRARY_PATH : "${onnxruntime}/lib"
    '';
  });
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
  llm-wrapper = pkgs.writeShellScriptBin "llm" (wrapWithLLMKeys "${llmWithPlugins}/bin/llm" [ ]);
  github-mcp-server-wrapper = pkgs.writeShellScriptBin "github-mcp-server" (
    wrapWithLLMKeys "${pkgs.github-mcp-server}/bin/github-mcp-server" [ ]
  );
  pi-wrapper = pkgs.writeShellScriptBin "pi" (wrapWithLLMKeys "${pi}/bin/pi" [ ]);
  vix-wrapper = pkgs.writeShellScriptBin "vix" (wrapWithLLMKeys "${vix}/bin/vix" [ ]);
  mistral-vibe-wrapper = pkgs.writeShellScriptBin "vibe" (
    wrapWithLLMKeys "${mistral-vibe}/bin/vibe" [ ]
  );
  #whisper-cpp =
  #  if cfg.cudaSupport then (pkgs.whisper-cpp.override { cudaSupport = true; }) else pkgs.whisper-cpp;
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
    ollama.enable = lib.mkEnableOption "";
  };
  config = lib.mkIf cfg.enable {
    services.ollama = lib.mkIf cfg.ollama.enable {
      enable = true;
      package = pkgs.ollama-cuda;
    };
    services.open-webui.enable = cfg.ollama.enable;
    services.open-webui.port = 11180;
    myhm = {
      sops.secrets.llm-keys = {
        mode = "0400";
        path = ".llm-keys";
      };

      home.sessionVariables = {
        PLAYWRIGHT_BROWSERS_PATH = "${pkgs.playwright.browsers}";
        CLAUDE_CONFIG_DIR = "$XDG_CONFIG_HOME/claude";
        CODEX_HOME = "$XDG_CONFIG_HOME/codex";
        T3CODE_HOME = "$XDG_CONFIG_HOME/t3code";
        PI_CODING_AGENT_DIR = "$XDG_CONFIG_HOME/pi/agent";
        PLANNOTATOR_DATA_DIR = "$XDG_CONFIG_HOME/plannotator";
        PLANNOTATOR_GLIMPSE = "0";
        PLANNOTATOR_SHARE = "disabled";
      };
      systemd.user.services.voxtype = lib.mkForce {
        Unit = {
          Description = "Voxtype push-to-talk voice-to-text daemon";
          Documentation = "https://voxtype.io";
          After = [
            "graphical-session.target"
            "pipewire.service"
            "pipewire-pulse.service"
          ];
          PartOf = [ "graphical-session.target" ];
        };
        Service = {
          Type = "simple";
          ExecStart = "${voxtype}/bin/voxtype daemon";
          Restart = "on-failure";
          RestartSec = 5;
        };
        Install = {
          WantedBy = [ "graphical-session.target" ];
        };
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
          dirge
          piper-tts
          espeak
          jujutsu
          #mcp-inspector
          llm-wrapper
          github-mcp-server-wrapper
          pi-wrapper
          vix-wrapper
          mistral-vibe-wrapper
          #codex
          #inputs.boxai.packages.${pkgs.stdenv.hostPlatform.system}.boxai
          cat-url-markdown
          inputs.tmux-buddy.packages.${pkgs.stdenv.hostPlatform.system}.default
          dotool # handy (speech to text) uses this for clipboard access
          wtype # handy (speech to text) uses this for clipboard access
          llm-agents.annot
          llm-agents.claude-code
          llm-agents.code-review-graph
          llm-agents.codex
          llm-agents.jscpd
          llm-agents.plannotator
          voxtype
          ccusage
          llm-agents.handy
          inputs.git-lines.packages.${pkgs.stdenv.hostPlatform.system}.default
          #pkgs.t3code
          universal-ctags
          difftastic
          ast-grep
          nushell
        ]
        ++ lib.optionals cfg.ollama.enable [
          ollama-cuda
          #whisper-cpp
        ];
    };
  };
}
