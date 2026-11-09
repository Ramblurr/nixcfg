{
  cfg,
  inputs,
  lib,
  pkgs,
  settings,
}:

let
  llmAgents = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
  # pi-hashline-edit-pro imports node:sqlite, which Bun's standalone runtime does not provide.
  piPackage = llmAgents.pi.override { useBun = false; };
in
{
  config = lib.mkIf cfg.enable {
    myhm =
      hm:
      let
        piSettings = settings hm.config.home.homeDirectory;
      in
      {
        xdg.configFile."pi/agent/settings.json".text = builtins.toJSON (
          piSettings
          // {
            packages = map toString piSettings.packages;
          }
        );
        xdg.configFile."pi/agent/models.json".text = builtins.toJSON {
          providers = {
            anthropic.apiKey = "!op read 'op://Private/Anthropic/api-key-quine'";
            typesafe.apiKey = "!op read 'op://Private/Jev API Keys/quine'";
            subq.apiKey = "!op read 'op://Private/Subq/quine-api-key'";
            openai.apiKey = "!op read 'op://Private/OpenAI/OPENAI_API_KEY_quine'";
            openrouter.apiKey = "!op read 'op://Private/OpenRouter/OPENROUTER_API_KEY'";
          };
        };
        xdg.configFile."pi/agent/pi-fff.json".text = builtins.toJSON {
          # FFF replaces find/grep and supplies @ file completion.
          mode = "override";
          enableHomeDirScanning = false;
          enableFsRootScanning = false;
        };
        home.packages = [ piPackage ];
      };
  };
}
