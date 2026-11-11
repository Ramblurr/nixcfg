{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  cfg = config.modules.dev.llms;
in
{
  config = lib.mkIf cfg.enable {
    myhm =
      hm:
      let
        home = hm.config.home.homeDirectory;
      in
      {
        imports = [ ./pi-nix.nix ];

        programs.pi = {
          enable = true;
          # pi-hashline-edit-pro imports node:sqlite, unavailable in Bun's standalone runtime.
          package = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.pi.override {
            useBun = false;
          };
          instances = {
            pi = {
              settings = {
                defaultProvider = "openai-codex";
                defaultModel = "gpt-6-astra";
                defaultThinkingLevel = "medium";
                skills = [
                  "${home}/src/git.outskirtslabs.com/casey/skills/skills/pi"
                  "${home}/src/git.outskirtslabs.com/casey/skills/skills/mine"
                  "${home}/src/git.outskirtslabs.com/casey/skills/skills/engineering"
                  "${home}/src/github.com/backnotprop/plannotator/apps/skills/core"
                  "${home}/src/github.com/backnotprop/plannotator/apps/skills/extra"
                ];
                packages = map toString [
                  pkgs.pi-reload
                  pkgs.pi-speak
                  #pkgs.pi-heartbeat
                  #pkgs.pi-ghost
                  "${home}/src/github.com/ramblurr/pi-extensions/handoff"
                  "${home}/src/github.com/ramblurr/pi-extensions/fork-new"
                  "${home}/src/github.com/ramblurr/pi-extensions/pi-subq"
                  pkgs.brepl-balance
                  #pkgs.pi-sexp-edit
                  #pkgs.pi-nrepl
                  #pkgs.pi-hashline-edit-pro
                  pkgs.plannotator-pi-extension
                  #pkgs.epimetheus
                  #pkgs.pi-mcp-adapter
                  pkgs.pi-openai-fast
                  pkgs.pi-cliproxy
                  #pkgs.pi-nono-sandbox
                  #pkgs.pi-matrix-relay
                  pkgs.pi-link
                  pkgs.pi-lens
                  pkgs.pi-fff
                  #pkgs.pi-goal-x
                ];
                npmCommand = [ "pnpm" ];
                hideThinkingBlock = true;
                theme = "dark";
                steeringMode = "all";
                showCacheMissNotices = true;
                shellCommandPrefix = "eval \"$(DEVSHELL_NO_MOTD=1 direnv export bash 2>/dev/null)\"";
              };
              models.providers = {
                anthropic.apiKey = "!op read 'op://Private/Anthropic/api-key-quine'";
                typesafe.apiKey = "!op read 'op://Private/Jev API Keys/quine'";
                subq.apiKey = "!op read 'op://Private/Subq/quine-api-key'";
                openai.apiKey = "!op read 'op://Private/OpenAI/OPENAI_API_KEY_quine'";
                openrouter.apiKey = "!op read 'op://Private/OpenRouter/OPENROUTER_API_KEY'";
                # Consumed by pkgs.pi-cliproxy; quine serves CLIProxyAPI to the tailnet.
                cliproxy = {
                  baseUrl = "https://quine.${config.repo.secrets.global.domain.tailnet}:8317";
                  apiKey = "!op read 'op://Private/CLIProxyAPI/api-key'";
                };
              };
              fff = {
                mode = "override";
                enableHomeDirScanning = false;
                enableFsRootScanning = false;
              };
            };
            pi-t3 = {
              settings = {
                defaultProvider = "openai-codex";
                defaultModel = "gpt-6-astra";
                defaultThinkingLevel = "medium";
                skills = [
                  "${home}/src/git.outskirtslabs.com/casey/skills/skills/t3"
                  "${home}/src/git.outskirtslabs.com/casey/skills/skills/mine"
                  "${home}/src/git.outskirtslabs.com/casey/skills/skills/engineering"
                ];
                packages = map toString [
                  pkgs.pi-reload
                  pkgs.pi-speak
                  "${home}/src/github.com/ramblurr/pi-extensions/handoff"
                  "${home}/src/github.com/ramblurr/pi-extensions/fork-new"
                  "${home}/src/github.com/ramblurr/pi-extensions/pi-subq"
                  pkgs.brepl-balance
                  pkgs.pi-openai-fast
                  pkgs.pi-lens
                  pkgs.pi-fff
                ];
                npmCommand = [ "pnpm" ];
                hideThinkingBlock = true;
                theme = "dark";
                steeringMode = "all";
                showCacheMissNotices = true;
                shellCommandPrefix = "eval \"$(DEVSHELL_NO_MOTD=1 direnv export bash 2>/dev/null)\"";
              };
              fff = {
                mode = "override";
                enableHomeDirScanning = false;
                enableFsRootScanning = false;
              };
            };
          };
        };
      };
  };
}
