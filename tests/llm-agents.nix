{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  evaluate =
    overrides:
    let
      system = lib.evalModules {
        specialArgs = { inherit inputs pkgs; };
        modules = [
          ../modules/dev/llms/agents.nix
          ../modules/dev/llms/clients.nix
          ../modules/dev/llms/pi.nix
          ({ lib, config, ... }: {
            options = {
              myhm = lib.mkOption { type = lib.types.deferredModule; };
              modules.dev.llms.enable = lib.mkEnableOption "agents";
              modules.dev.llms.onepasswordConnect = {
                tokenFile = lib.mkOption {
                  type = lib.types.nullOr lib.types.str;
                  default = null;
                };
                package = lib.mkOption {
                  type = lib.types.package;
                  default = pkgs.hello;
                };
              };
              repo.secrets.global.domain.tailnet = lib.mkOption {
                type = lib.types.str;
                default = "example.test";
              };
            };
            config.modules.dev.llms = lib.recursiveUpdate {
              enable = true;
              agents.extraText = "Shared rule.";
              skills.enable = true;
              codex.marketplaces = lib.optional config.modules.dev.llms.skills.enable config.modules.dev.llms.skills.package.codexPlugin;
              codex.plugins = lib.optional config.modules.dev.llms.skills.enable "ol-skills@ol-skills";
              pi = {
                package = pkgs.hello;
                agents.preamble = "";
                instances = {
                  pi.settings.theme = "host-theme";
                  pi-t3.settings.theme = "t3-theme";
                };
              };
              codex.settings = {
                model_provider = if config.modules.dev.llms.proxy.enable then "cliproxy" else "openai";
              }
              // lib.optionalAttrs config.modules.dev.llms.proxy.enable {
                model_providers.cliproxy.env_key = "CLIPROXY_API_KEY";
              };
              codex.removeSettings =
                lib.optionals config.modules.dev.llms.proxy.removeLegacyCredentials [
                  [
                    "model_providers"
                    "cliproxy"
                    "experimental_bearer_token"
                  ]
                ]
                ++ lib.optionals (!config.modules.dev.llms.proxy.enable) [
                  [
                    "model_providers"
                    "cliproxy"
                  ]
                ];
              claude.settings = lib.optionalAttrs config.modules.dev.llms.proxy.enable {
                env.ANTHROPIC_BASE_URL = config.modules.dev.llms.proxy.url;
              };
              claude.removeSettings =
                lib.optionals config.modules.dev.llms.proxy.removeLegacyCredentials [
                  [
                    "env"
                    "ANTHROPIC_AUTH_TOKEN"
                  ]
                ]
                ++ lib.optionals (!config.modules.dev.llms.proxy.enable) [
                  [
                    "env"
                    "ANTHROPIC_BASE_URL"
                  ]
                ];
              claude.agents.preamble = "";
              codex.agents.preamble = "";
              proxy = {
                enable = true;
                credential = "op://fixture/proxy/key";
                url = "https://proxy.example.test:8317/";
                removeLegacyCredentials = true;
              };
            } overrides;
          })
        ];
      };
    in
    (inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [
        system.config.myhm
        {
          home.username = "test";
          home.homeDirectory = "/home/test";
          home.stateVersion = "26.05";
        }
      ];
    }).config;
  common = evaluate { };
  custom = evaluate {
    claude.agents.extraText = "Claude only.";
    codex = {
      agents.extraText = "Codex only.";
      settings.model = "custom-model";
    };
  };
  disabled = evaluate {
    codex.enable = false;
    claude.enable = false;
  };
  compatible = evaluate { proxy.removeLegacyCredentials = false; };
  unmanagedInstructions = evaluate { agents.enable = false; };
  codexInstructionsOff = evaluate { codex.agents.enable = false; };
  withPreamble = evaluate { codex.agents.preamble = "Preamble."; };
  withFiles = evaluate {
    agents.extraFiles = [ (builtins.toFile "shared-instructions.md" "Shared file rule.") ];
    codex.agents.extraFiles = [ (builtins.toFile "codex-instructions.md" "Codex file rule.") ];
  };
  direct = evaluate {
    proxy.enable = false;
    skills.enable = false;
  };
  wrapped =
    reader:
    evaluate {
      skills.enable = false;
      claude.enable = false;
      codex.extraArgs = [ "--fixture-argument" ];
      codex.package = pkgs.writeShellScriptBin "codex" ''
        test "$1" = --fixture-argument || exit 1
        test "$CLIPROXY_API_KEY" = fixture-token || exit 1
        echo invoked
      '';
      onepasswordConnect = {
        tokenFile = "/fixture";
        package = reader;
      };
    };
  wrapper =
    reader:
    lib.getExe (lib.findFirst (p: (p.name or "") == "codex") null (wrapped reader).home.packages);
  goodWrapper = wrapper (pkgs.writeShellScriptBin "reader" "echo fixture-token");
  badWrapper = wrapper (pkgs.writeShellScriptBin "reader" "exit 1");
  script =
    config: tool: pkgs.writeText "${tool}-activation" config.home.activation.${tool + "Settings"}.data;
in
assert
  common.xdg.configFile."codex/AGENTS.md".text == common.xdg.configFile."claude/CLAUDE.md".text;
assert custom.xdg.configFile."codex/AGENTS.md".text == "Shared rule.\n\nCodex only.";
assert custom.xdg.configFile."claude/CLAUDE.md".text == "Shared rule.\n\nClaude only.";
assert !(disabled.xdg.configFile ? "codex/AGENTS.md");
assert !(disabled.xdg.configFile ? "claude/CLAUDE.md");
assert !(unmanagedInstructions.xdg.configFile ? "codex/AGENTS.md");
assert !(unmanagedInstructions.xdg.configFile ? "claude/CLAUDE.md");
assert !(codexInstructionsOff.xdg.configFile ? "codex/AGENTS.md");
assert codexInstructionsOff.xdg.configFile ? "claude/CLAUDE.md";
assert withPreamble.xdg.configFile."codex/AGENTS.md".text == "Preamble.\n\nShared rule.";
assert
  withFiles.xdg.configFile."codex/AGENTS.md".text
  == "Shared rule.\n\nShared file rule.\n\nCodex file rule.";
assert withFiles.xdg.configFile."claude/CLAUDE.md".text == "Shared rule.\n\nShared file rule.";
assert
  withFiles.home.file."/home/test/.config/pi/agent/AGENTS.md".text
  == "Shared rule.\n\nShared file rule.";
assert
  withFiles.home.file."/home/test/.config/pi-t3/agent/AGENTS.md".text
  == "Shared rule.\n\nShared file rule.";
assert !(direct.home.activation ? codexLocalSkills);
assert lib.hasInfix "plugin add" common.home.activation.codexLocalSkills.data;
assert lib.hasInfix "experimental_bearer_token" common.home.activation.codexSettings.data;
assert common.home.file."/home/test/.config/pi/agent/AGENTS.md".text == "Shared rule.";
assert
  (builtins.fromJSON common.home.file."/home/test/.config/pi/agent/settings.json".text).theme
  == "host-theme";
assert
  (builtins.fromJSON common.home.file."/home/test/.config/pi-t3/agent/settings.json".text).theme
  == "t3-theme";
pkgs.runCommand "llm-agents-test"
  {
    nativeBuildInputs = [
      pkgs.bash
      pkgs.jaq
      pkgs.jq
    ];
  }
  ''
      set -euo pipefail
    test "$(${goodWrapper})" = invoked
    if ${badWrapper} > bad.out 2> bad.err; then exit 1; fi
    test ! -s bad.out
      errorEcho() { echo "$@" >&2; }
      export -f errorEcho
      test_home="$TMPDIR/home"
      mkdir -p "$test_home/.config/claude" "$test_home/.config/codex"
      printf '%s\n' '{"customSetting":true,"env":{"ANTHROPIC_AUTH_TOKEN":"fixture-old-token"}}' > "$test_home/.config/claude/settings.json"
      printf '%s\n' '[projects."/example"]' 'trust_level="trusted"' '[model_providers.cliproxy]' 'experimental_bearer_token="fixture-old-token"' > "$test_home/.config/codex/config.toml"
      chmod 600 "$test_home/.config/"{claude/settings.json,codex/config.toml}
      for source in ${script compatible "codex"} ${script compatible "claude"}; do
      sed "s|/home/test|$test_home|g" "$source" > activate.sh
      bash activate.sh
    done
    jq -e '.env.ANTHROPIC_AUTH_TOKEN == "fixture-old-token"' "$test_home/.config/claude/settings.json"
    jaq --from toml -e '.model_providers.cliproxy.experimental_bearer_token == "fixture-old-token"' "$test_home/.config/codex/config.toml"
    for source in ${script custom "codex"} ${script custom "claude"}; do
        sed "s|/home/test|$test_home|g" "$source" > activate.sh
        bash activate.sh
      done
      jq -e '.customSetting and (.env.ANTHROPIC_AUTH_TOKEN == null) and (.env.ANTHROPIC_BASE_URL == "https://proxy.example.test:8317")' "$test_home/.config/claude/settings.json"
      jaq --from toml -e '.model == "custom-model" and .projects["/example"].trust_level == "trusted" and .model_providers.cliproxy.env_key == "CLIPROXY_API_KEY" and .model_providers.cliproxy.experimental_bearer_token == null' "$test_home/.config/codex/config.toml"
      for source in ${script direct "codex"} ${script direct "claude"}; do
        sed "s|/home/test|$test_home|g" "$source" > activate.sh
        bash activate.sh
      done
      jq -e '.env.ANTHROPIC_BASE_URL == null and .customSetting' "$test_home/.config/claude/settings.json"
      jaq --from toml -e '.model_provider == "openai" and .model_providers.cliproxy == null and .projects["/example"].trust_level == "trusted"' "$test_home/.config/codex/config.toml"
      touch "$out"
  ''
