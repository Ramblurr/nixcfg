{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  devCfg = config.modules.dev;
  cfg = devCfg.random;
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
    wrapWithLLMKeys "${pkgs.opencode}/bin/opencode" [ "ANTHROPIC_API_KEY" ]
  );
  gemini-cli-wrapper = pkgs.writeShellScriptBin "gemini-cli" (
    wrapWithLLMKeys "${pkgs.gemini-cli}/bin/gemini-cli" [ ]
  );
in
{
  config = lib.mkIf cfg.enable {
    myhm = {
      sops.secrets.llm-keys = {
        mode = "0400";
        path = ".llm-keys";
      };
      home.packages = with pkgs; [
        mcp-inspector
        claude-code
        llm
        gemini-cli-wrapper
        ccusage
        inputs.boxai.packages.${pkgs.system}.boxai
        crush-wrapper
        opencode-wrapper
      ];
    };
  };
}
