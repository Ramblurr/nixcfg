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
      (toString pkgs.pi-reload)
      (toString pkgs.pi-ghost)
      "${home}/src/github.com/ramblurr/pi-extensions/handoff"
      "${home}/src/github.com/ramblurr/pi-extensions/fork-new"
      (toString pkgs.brepl-balance)
      (toString pkgs.pi-sexp-edit)
      (toString pkgs.pi-nrepl)
      (toString pkgs.pi-hashline-edit)
      (toString pkgs.plannotator-pi-extension)
      (toString pkgs.epimetheus)
      (toString pkgs.pi-mcp-adapter)
      (toString pkgs.pi-openai-fast)
      "${home}/src/github.com/ramblurr/pi-extensions/pi-nono-sandbox"
      "${home}/src/github.com/ramblurr/pi-matrix-relay/extension"
      (toString pkgs.pi-link)
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
