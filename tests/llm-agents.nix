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
          (
            { lib, ... }:
            {
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
                pi.package = pkgs.hello;
              } overrides;
            }
          )
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

  # Records each Codex CLI call so the test can check what the activation asked of it.
  loggingCodex = pkgs.writeShellScriptBin "codex" ''echo "$*" >> "$CODEX_HOME/calls"'';
  marketplace = pkgs.writeTextDir ".agents/plugins/marketplace.json" (
    builtins.toJSON {
      name = "fixture";
      plugins = [ ];
    }
  );

  # Every instruction source is set, so the expected text pins their order and the dropped empties.
  configured = evaluate {
    agents = {
      preamble = "Shared preamble.";
      extraText = "Shared rule.";
      extraFiles = [ (builtins.toFile "shared.md" "Shared file.") ];
    };
    codex.agents = {
      preamble = "Codex preamble.";
      extraText = "Codex rule.";
      extraFiles = [ (builtins.toFile "codex.md" "Codex file.") ];
    };
    claude.agents.preamble = "";
    pi.agents.preamble = "Pi preamble.";
    pi.settings = {
      theme = "shared-theme";
      quietStartup = true;
    };
    pi.instances = {
      pi = { };
      pi-t3.settings.theme = "t3-theme";
    };
    codex = {
      package = loggingCodex;
      marketplaces = [ marketplace ];
      plugins = [ "my@fixture" ];
      settings.model = "fixture-model";
      removeSettings = [
        [
          "model_providers"
          "legacy"
        ]
      ];
    };
    claude = {
      settings.env.ANTHROPIC_BASE_URL = "https://proxy.example.test";
      removeSettings = [
        [
          "env"
          "ANTHROPIC_AUTH_TOKEN"
        ]
      ];
    };
  };
  instructionsOff = evaluate { agents.enable = false; };
  codexInstructionsOff = evaluate { codex.agents.enable = false; };

  wrapper =
    reader:
    let
      config = evaluate {
        claude.enable = false;
        proxy = {
          enable = true;
          credential = "op://fixture/proxy/key";
          url = "https://proxy.example.test:8317/";
        };
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
    in
    lib.getExe (lib.findFirst (p: (p.name or "") == "codex") null config.home.packages);
  goodWrapper = wrapper (pkgs.writeShellScriptBin "reader" "echo fixture-token");
  badWrapper = wrapper (pkgs.writeShellScriptBin "reader" "exit 1");

  activation = name: pkgs.writeText name configured.home.activation.${name}.data;
  piFile = instance: name: configured.home.file."/home/test/.config/${instance}/agent/${name}".text;
in
assert
  configured.xdg.configFile."codex/AGENTS.md".text
  == "Shared preamble.\n\nCodex preamble.\n\nShared rule.\n\nCodex rule.\n\nShared file.\n\nCodex file.";
assert
  configured.xdg.configFile."claude/CLAUDE.md".text
  == "Shared preamble.\n\nShared rule.\n\nShared file.";
assert
  piFile "pi" "AGENTS.md" == "Shared preamble.\n\nPi preamble.\n\nShared rule.\n\nShared file.";
assert piFile "pi-t3" "AGENTS.md" == piFile "pi" "AGENTS.md";
assert
  lib.getAttrs [ "theme" "quietStartup" ] (builtins.fromJSON (piFile "pi" "settings.json")) == {
    theme = "shared-theme";
    quietStartup = true;
  };
assert
  lib.getAttrs [ "theme" "quietStartup" ] (builtins.fromJSON (piFile "pi-t3" "settings.json")) == {
    theme = "t3-theme";
    quietStartup = true;
  };
assert !(instructionsOff.xdg.configFile ? "codex/AGENTS.md");
assert !(instructionsOff.xdg.configFile ? "claude/CLAUDE.md");
assert !(codexInstructionsOff.xdg.configFile ? "codex/AGENTS.md");
assert codexInstructionsOff.xdg.configFile ? "claude/CLAUDE.md";
assert !(instructionsOff.home.activation ? codexLocalSkills);
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

    # The launcher passes the proxy key and extra arguments, and never runs Codex without the key.
    test "$(${goodWrapper})" = invoked
    if ${badWrapper} > bad.out 2> bad.err; then exit 1; fi
    test ! -s bad.out

    errorEcho() { echo "$@" >&2; }
    export -f errorEcho
    home="$TMPDIR/home"
    mkdir -p "$home/.config/claude" "$home/.config/codex"
    printf '%s\n' '{"customSetting":true,"env":{"ANTHROPIC_AUTH_TOKEN":"old-token"}}' > "$home/.config/claude/settings.json"
    printf '%s\n' '[projects."/example"]' 'trust_level="trusted"' '[model_providers.legacy]' 'base_url="old"' > "$home/.config/codex/config.toml"
    chmod 600 "$home/.config/"{claude/settings.json,codex/config.toml}
    for script in ${activation "claudeSettings"} ${activation "codexSettings"} ${activation "codexLocalSkills"}; do
      sed "s|/home/test|$home|g" "$script" > activate.sh
      bash activate.sh
    done

    # Settings merge into the user's file, keep their own keys, and drop the removed paths.
    jq -e '.customSetting and .env.ANTHROPIC_AUTH_TOKEN == null and .env.ANTHROPIC_BASE_URL == "https://proxy.example.test"' "$home/.config/claude/settings.json"
    jaq --from toml -e '.model == "fixture-model" and .projects["/example"].trust_level == "trusted" and .model_providers.legacy == null' "$home/.config/codex/config.toml"

    # Each marketplace is replaced by name, then the declared plugins are installed.
    printf '%s\n' \
      'plugin marketplace remove fixture' \
      'plugin marketplace add ${marketplace} --json' \
      'plugin add my@fixture --json' > expected-calls
    diff expected-calls "$home/.config/codex/calls"

    touch "$out"
  ''
