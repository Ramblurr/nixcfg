{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  devCfg = config.modules.dev;
  cfg = config.modules.dev.llms;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
  wrapWithLLMKeys = cmd: removeVars: ''
    #!${pkgs.runtimeShell}
    if [ -f "$HOME/.llm-keys" ]; then
      set -a  # automatically export all variables
      source "$HOME/.llm-keys"
      set +a  # turn off automatic export
      ${lib.concatStringsSep "\n" (map (v: "unset ${v}") removeVars)}
    fi
    exec ${cmd} "$@"
  '';
  opencode = inputs.llm-agents.packages.${pkgs.system}.opencode;
  crush = inputs.llm-agents.packages.${pkgs.system}.crush;
  gemini-cli = inputs.llm-agents.packages.${pkgs.system}.gemini-cli;
  crush-wrapper = pkgs.writeShellScriptBin "crush" (wrapWithLLMKeys "${crush}/bin/crush" [ ]);
  opencode-wrapper = pkgs.writeShellScriptBin "opencode" (
    wrapWithLLMKeys "${opencode}/bin/opencode" [
      "ANTHROPIC_API_KEY"
    ]
  );
  llm-wrapper = pkgs.writeShellScriptBin "llm" (wrapWithLLMKeys "${pkgs.llm}/bin/llm" [ ]);
  gemini-cli-wrapper = pkgs.writeShellScriptBin "gemini" (
    wrapWithLLMKeys "${gemini-cli}/bin/gemini" [ ]
  );
  github-mcp-server-wrapper = pkgs.writeShellScriptBin "github-mcp-server" (
    wrapWithLLMKeys "${pkgs.github-mcp-server}/bin/github-mcp-server" [ ]
  );

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
    myhm = {
      sops.secrets.llm-keys = {
        mode = "0400";
        path = ".llm-keys";
      };
      home.packages =
        with pkgs;
        [
          jujutsu
          mcp-inspector
          llm-wrapper
          github-mcp-server-wrapper
          gemini-cli-wrapper
          #codex
          #inputs.boxai.packages.${pkgs.system}.boxai
          opencode-wrapper
          inputs.beads.packages.${pkgs.stdenv.hostPlatform.system}.default
          (pkgs.writeShellScriptBin "cat-url-markdown" ''
            if [ -z "$1" ]; then
              echo "usage: $(basename "$0") URL [FILENAME]"
              exit 1
            fi
            curl -sSL --output - $(printf "https://r.jina.ai/%s" $1)
          '')
          (if cfg.cudaSupport == true then (whisper-cpp.override { cudaSupport = true; }) else whisper-cpp)
        ]
        ++ (with inputs.llm-agents.packages.${pkgs.system}; [
          claude-code
          ccusage
          copilot-cli
          handy
        ]);
    };
  };
}
