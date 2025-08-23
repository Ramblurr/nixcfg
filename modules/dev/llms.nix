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
  crush-wrapper = pkgs.writeShellScriptBin "crush" (wrapWithLLMKeys "${pkgs.crush}/bin/crush" [ ]);
  opencode-wrapper = pkgs.writeShellScriptBin "opencode" (
    wrapWithLLMKeys "${pkgs.opencode-bin}/bin/opencode" [ "ANTHROPIC_API_KEY" ]
  );
  llm-wrapper = pkgs.writeShellScriptBin "llm" (wrapWithLLMKeys "${pkgs.llm}/bin/llm" [ ]);
  gemini-cli-wrapper = pkgs.writeShellScriptBin "gemini-cli" (
    wrapWithLLMKeys "${pkgs.gemini-cli}/bin/gemini-cli" [ ]
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
    myhm = {
      sops.secrets.llm-keys = {
        mode = "0400";
        path = ".llm-keys";
      };
      home.packages = with pkgs; [
        mcp-inspector
        claude-code
        llm-wrapper
        gemini-cli-wrapper
        ccusage
        inputs.boxai.packages.${pkgs.system}.boxai
        crush-wrapper
        opencode-wrapper
        (pkgs.writeShellScriptBin "cat-url-markdown" ''
          if [ -z "$1" ]; then
            echo "usage: $(basename "$0") URL [FILENAME]"
            exit 1
          fi
          curl -sSL --output - $(printf "https://r.jina.ai/%s" $1)
        '')
        (if cfg.cudaSupport == true then (whisper-cpp.override { cudaSupport = true; }) else whisper-cpp)
      ];
    };
  };
}
