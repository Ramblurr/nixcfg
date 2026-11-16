{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  cfg = config.modules.dev.llms;
  connect = cfg.onepasswordConnect;
  # Reads LLM-vault keys through Connect so agents start without desktop approval.
  opRead =
    if connect.tokenFile == null then
      "op read"
    else
      toString (
        pkgs.writeShellScript "op-read-llm" ''
          OP_CONNECT_HOST=${lib.escapeShellArg connect.host} \
          OP_CONNECT_TOKEN="$(< ${lib.escapeShellArg connect.tokenFile})" \
            exec ${lib.getExe pkgs._1password-cli} read "$@"
        ''
      );
in
{
  config = lib.mkIf cfg.enable {
    myhm =
      hm:
      let
        home = hm.config.home.homeDirectory;
        cliproxyProvider = {
          baseUrl = "https://quine.${config.repo.secrets.global.domain.tailnet}:8317";
          apiKey = "!${opRead} 'op://LLM/CLIProxyAPI/credential'";
        };
        all-providers = {
          anthropic.apiKey = "!${opRead} 'op://LLM/Anthropic-quine/credential'";
          typesafe.apiKey = "!${opRead} 'op://LLM/Jev-quine/credential'";
          subq.apiKey = "!${opRead} 'op://LLM/Subq-quine/credential'";
          openai.apiKey = "!${opRead} 'op://LLM/OpenAI-quine/credential'";
          openrouter.apiKey = "!${opRead} 'op://LLM/OpenRouter/credential'";
          cliproxy = cliproxyProvider;
        };
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
                defaultProvider = "cliproxy";
                defaultModel = "gpt-6.1-sol";
                defaultThinkingLevel = "high";
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
              models.providers = all-providers;
              fff = {
                mode = "override";
                enableHomeDirScanning = false;
                enableFsRootScanning = false;
              };
            };
            pi-t3 = {
              settings = {
                defaultProvider = "cliproxy";
                defaultModel = "gpt-6.1-sol";
                defaultThinkingLevel = "high";
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
                  pkgs.pi-cliproxy
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
              models.providers = all-providers;
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
