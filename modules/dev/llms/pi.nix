{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  cfg = config.modules.dev.llms;
  settings = home: {
    defaultProvider = "openai-codex";
    defaultModel = "gpt-5.6-sol";
    defaultThinkingLevel = "high";
    skills = [
      "${home}/src/github.com/ramblurr/nix-devenv/skills/pi"
      "${home}/src/github.com/ramblurr/nix-devenv/skills/mine"
      "${home}/src/github.com/ramblurr/nix-devenv/skills/engineering"
      "${home}/src/github.com/backnotprop/plannotator/apps/skills/core"
      "${home}/src/github.com/backnotprop/plannotator/apps/skills/extra"
    ];
    packages = [
      pkgs.pi-reload
      pkgs.pi-heartbeat
      pkgs.pi-ghost
      "${home}/src/github.com/ramblurr/pi-extensions/handoff"
      "${home}/src/github.com/ramblurr/pi-extensions/fork-new"
      pkgs.brepl-balance
      pkgs.pi-sexp-edit
      pkgs.pi-nrepl
      pkgs.pi-hashline-edit-pro
      pkgs.plannotator-pi-extension
      pkgs.epimetheus
      pkgs.pi-mcp-adapter
      pkgs.pi-openai-fast
      pkgs.pi-nono-sandbox
      pkgs.pi-matrix-relay
      pkgs.pi-link
    ];
    npmCommand = [ "pnpm" ];
    hideThinkingBlock = true;
    theme = "dark";
    steeringMode = "all";
    showCacheMissNotices = true;
    shellCommandPrefix = "eval \"$(DEVSHELL_NO_MOTD=1 direnv export bash 2>/dev/null)\"";
  };
in
import ./pi-nix.nix {
  inherit
    cfg
    inputs
    lib
    pkgs
    settings
    ;
}
