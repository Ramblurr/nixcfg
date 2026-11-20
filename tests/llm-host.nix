{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  config = {
    modules.dev.llms = suite;
    modules.users.primaryUser.homeDirectory = "/home/test";
  };
  host = import ../hosts/quine/llm.nix {
    inherit
      config
      inputs
      pkgs
      lib
      ;
  };
  profile = import ../modules/dev/llms/profile.nix {
    inherit
      config
      inputs
      pkgs
      lib
      ;
  };
  suite = lib.foldl' lib.recursiveUpdate { } [
    {
      codex.enable = true;
      claude.enable = true;
      pi.enable = true;
      onepasswordConnect.tokenFile = null;
      proxy = {
        removeLegacyCredentials = false;
        url = "https://quine.example.test:8317";
        credential = "op://LLM/CLIProxyAPI/credential";
      };
      skills.package = pkgs.harness-plugins;
      profile.codexSessionHookUsers = [ ];
    }
    host.modules.dev.llms
    profile.config.content.modules.dev.llms
  ];
  cliproxyModelIds = map (model: model.id) suite.pi.instances.pi.models.providers.cliproxy.models;
in
assert lib.all (instance: lib.elem instance.settings.defaultModel cliproxyModelIds) (
  lib.attrValues suite.pi.instances
);
assert suite.codex.settings.model == "gpt-6-luna";
assert suite.codex.settings.web_search == "disabled";
assert !(suite.codex.settings.model_providers.cliproxy ? env_key);
assert lib.elem "model_providers.cliproxy.env_key=\"CLIPROXY_API_KEY\"" suite.codex.extraArgs;
assert suite.claude.settings.modelSettings."claude-opus-5".effortLevel == "high";
assert suite.claude.settings.env.ANTHROPIC_BASE_URL == suite.proxy.url;
assert suite.t3.settings.providerInstances.pi.enabled;
assert suite.t3.settings.providerInstances.codex.config.homePath == "/home/test/.config/codex";
assert
  suite.nono.profiles.eca.network.custom_credentials.openrouter.upstream == "${suite.proxy.url}/v1";
assert suite.nono.ecaSettings.providers.openrouter.models ? "google/gemini-4-carbon";
assert suite.pi.instances.pi.settings.defaultProvider == "cliproxy";
assert suite.pi.instances.pi-t3.settings.defaultThinkingLevel == "high";
assert lib.all (lib.hasPrefix builtins.storeDir) suite.pi.instances.pi.settings.skills;
assert lib.elem [ "features" "skills" ] suite.codex.removeSettings;
pkgs.runCommand "llm-host-settings-test" { } ''touch "$out"''
