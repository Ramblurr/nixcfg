{ inputs, pkgs }:
let
  inherit (pkgs) lib;
  evaluateSystem =
    overrides:
    lib.evalModules {
      specialArgs = { inherit inputs pkgs; };
      modules = [
        ../modules/dev/llms/agents.nix
        ../modules/dev/llms/clients.nix
        ../modules/dev/llms/pi.nix
        ../modules/dev/llms/profile.nix
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
              environment = lib.mkOption { type = lib.types.anything; };
              modules.dev.llms.t3 = lib.mkOption { type = lib.types.anything; };
              modules.users.primaryUser.homeDirectory = lib.mkOption { default = "/home/test"; };
            };
            config.modules.dev.llms = lib.recursiveUpdate {
              enable = true;
              pi.package = pkgs.hello;
            } overrides;
          }
        )
      ];
    };
  evaluate =
    overrides:
    (inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [
        (evaluateSystem overrides).config.myhm
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

  profile =
    (evaluateSystem {
      profile.enable = true;
      proxy.enable = true;
    }).config.modules.dev.llms;
  cliproxyModelIds = map (model: model.id) profile.pi.instances.pi.models.providers.cliproxy.models;
  rpcProfile =
    (evaluateSystem {
      profile = {
        enable = true;
        piProviderCredentials.subq = "op://fixture/subq/key";
        piProxyExtraModels = [
          (
            (import ../modules/dev/llms/models.nix).subq.savant
            // {
              baseUrl = "http://proxy.invalid/configured/v1";
            }
          )
          {
            id = "fixture-mistral";
            api = "mistral-conversations";
          }
        ];
      };
      proxy = {
        enable = true;
        url = "http://proxy.invalid";
        credential = "op://fixture/proxy/key";
      };
      onepasswordConnect = {
        tokenFile = "/fixture";
        package = pkgs.writeShellScriptBin "reader" ''
          test "$1" = op://fixture/proxy/key || exit 1
          echo fixture-token
        '';
      };
    }).config.modules.dev.llms;
  rpcProfiles = pkgs.writeText "pi-proxy-rpc-profiles.json" (
    builtins.toJSON (
      lib.mapAttrs (_: instance: {
        inherit (instance) models;
        settings = {
          inherit (instance.settings) defaultProvider defaultModel;
          packages = lib.filter (package: package == toString pkgs.pi-subq) instance.settings.packages;
        };
      }) rpcProfile.pi.instances
    )
  );
  pi = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.pi.override { useBun = false; };

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
assert lib.all (instance: lib.elem instance.settings.defaultModel cliproxyModelIds) (
  lib.attrValues profile.pi.instances
);
assert lib.all (
  instance:
  let
    savant = lib.findFirst (
      model: model.id == "subq-savant-1.4"
    ) null instance.models.providers.cliproxy.models;
  in
  savant.api == "openai-completions"
  &&
    savant.cost == {
      input = 0.10;
      output = 0.50;
      cacheRead = 0.01;
      cacheWrite = 0;
    }
) (lib.attrValues rpcProfile.pi.instances);
assert lib.all (id: lib.elem id cliproxyModelIds) [
  "subq-savant-1.4"
  "mistral-large-4"
];
# Every model comes from Nix; no profile loads a discovery extension.
assert lib.all (
  instance:
  lib.elem (toString pkgs.pi-subq) instance.settings.packages
  && !(lib.any (package: lib.hasInfix "pi-cliproxy" (toString package)) instance.settings.packages)
  && instance.settings.defaultTools == [ "+codemode" ]
) (lib.attrValues rpcProfile.pi.instances);
assert lib.all (
  model: lib.hasPrefix "http://proxy.invalid" model.baseUrl
) rpcProfile.pi.instances.pi.models.providers.cliproxy.models;
assert profile.codex.settings.model_providers.cliproxy.env_key == "CLIPROXY_API_KEY";
assert lib.hasSuffix "/bin/agy" profile.t3.settings.providerInstances.antigravity.config.binaryPath;
assert lib.elem [
  "model_providers"
  "cliproxy"
  "experimental_bearer_token"
] profile.codex.removeSettings;
assert lib.elem [ "env" "ANTHROPIC_AUTH_TOKEN" ] profile.claude.removeSettings;
assert profile.claude.settings.env.ANTHROPIC_BASE_URL == profile.proxy.url;
assert lib.all (lib.hasPrefix builtins.storeDir) profile.pi.instances.pi.settings.skills;
pkgs.runCommand "llm-agents-test"
  {
    nativeBuildInputs = [
      pkgs.bash
      pkgs.jaq
      pkgs.jq
      pkgs.python3
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

    python3 ${./pi-proxy-rpc.py} ${lib.getExe pi} ${rpcProfiles}

    touch "$out"
  ''
