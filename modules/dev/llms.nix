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
        gemini-cli
        ccusage
        inputs.boxai.packages.${pkgs.system}.boxai
      ];
    };
  };
}
