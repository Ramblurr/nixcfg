{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.dev.llms;
  packages = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
  json = pkgs.formats.json { };
  toml = pkgs.formats.toml { };
  wrap =
    name: package: variable: extraArgs:
    pkgs.writeShellScriptBin name ''
      ${lib.optionalString cfg.proxy.enable "export ${variable}=tailnet"}
      exec ${lib.getExe package} ${lib.escapeShellArgs extraArgs} "$@"
    '';
  codexPackage =
    if cfg.proxy.enable || cfg.codex.extraArgs != [ ] then
      wrap "codex" cfg.codex.package "CLIPROXY_API_KEY" cfg.codex.extraArgs
    else
      cfg.codex.package;
  claudePackage =
    if cfg.proxy.enable || cfg.claude.extraArgs != [ ] then
      wrap "claude" cfg.claude.package "ANTHROPIC_AUTH_TOKEN" cfg.claude.extraArgs
    else
      cfg.claude.package;
  instructions =
    tool:
    lib.concatStringsSep "\n\n" (
      lib.filter (s: s != "") (
        [
          cfg.agents.preamble
          tool.agents.preamble
          cfg.agents.extraText
          tool.agents.extraText
        ]
        ++ map builtins.readFile (cfg.agents.extraFiles ++ tool.agents.extraFiles)
      )
    );
in
{
  options.modules.dev.llms = {
    codex.package = lib.mkOption {
      type = lib.types.package;
      default = packages.codex;
      description = "Underlying Codex CLI package.";
    };
    claude.package = lib.mkOption {
      type = lib.types.package;
      default = packages.claude-code;
      description = "Underlying Claude Code package.";
    };
    codex.finalPackage = lib.mkOption {
      type = lib.types.package;
      readOnly = true;
      default = codexPackage;
      description = "Codex package with the proxy placeholder key and host arguments.";
    };
    claude.finalPackage = lib.mkOption {
      type = lib.types.package;
      readOnly = true;
      default = claudePackage;
      description = "Claude package with the proxy placeholder key and host arguments.";
    };
  };
  config = lib.mkIf cfg.enable {
    myhm =
      hm:
      let
        codexHome = "${hm.config.xdg.configHome}/codex";
        claudeHome = "${hm.config.xdg.configHome}/claude";
        merge =
          format: path: settings: operation:
          hm.lib.hm.generators.mkImpureConfigMerger {
            inherit pkgs format path;
            empty = "{}";
            mode = "600";
            staticSettings =
              (if format == "json" then json else toml).generate "agent-settings.${format}"
                settings;
            jqOperation = operation;
          };
      in
      {
        home.packages =
          lib.optional cfg.codex.enable codexPackage ++ lib.optional cfg.claude.enable claudePackage;
        home.sessionVariables =
          lib.optionalAttrs cfg.codex.enable { CODEX_HOME = codexHome; }
          // lib.optionalAttrs cfg.claude.enable { CLAUDE_CONFIG_DIR = claudeHome; };
        xdg.configFile =
          lib.optionalAttrs (cfg.codex.enable && cfg.agents.enable && cfg.codex.agents.enable) {
            "codex/AGENTS.md".text = instructions cfg.codex;
          }
          // lib.optionalAttrs (cfg.claude.enable && cfg.agents.enable && cfg.claude.agents.enable) {
            "claude/CLAUDE.md".text = instructions cfg.claude;
          }
          // lib.optionalAttrs cfg.claude.enable (
            lib.mapAttrs' (name: source: {
              name = "claude/skills/${name}";
              value = { inherit source; };
            }) cfg.claude.plugins
          );
        home.activation =
          lib.optionalAttrs cfg.codex.enable {
            codexSettings = hm.lib.hm.dag.entryAfter [ "linkGeneration" ] (
              merge "toml" "${codexHome}/config.toml" cfg.codex.settings
                "$dynamic * $static | delpaths(${builtins.toJSON cfg.codex.removeSettings})"
            );
          }
          // lib.optionalAttrs cfg.claude.enable {
            claudeSettings = hm.lib.hm.dag.entryAfter [ "linkGeneration" ] (
              merge "json" "${claudeHome}/settings.json" cfg.claude.settings
                "$dynamic * $static | delpaths(${builtins.toJSON cfg.claude.removeSettings})"
            );
          }
          //
            lib.optionalAttrs (cfg.codex.enable && (cfg.codex.plugins != [ ] || cfg.codex.marketplaces != [ ]))
              {
                codexLocalSkills = hm.lib.hm.dag.entryAfter [ "codexSettings" ] ''
                  if [[ -v DRY_RUN ]]; then
                    echo "Would install declared Codex plugins"
                  else
                    ${lib.concatMapStringsSep "\n" (
                      source:
                      let
                        codex = "CODEX_HOME=${lib.escapeShellArg codexHome} ${lib.getExe cfg.codex.package}";
                        root = lib.escapeShellArg (toString source);
                      in
                      # Codex refuses to re-add a marketplace name from a new store path, and keeps a
                      # cached plugin whose version is unchanged, so replace the marketplace each time.
                      ''
                        ${codex} plugin marketplace remove "$(${lib.getExe pkgs.jq} -r .name ${root}/.agents/plugins/marketplace.json)" >/dev/null 2>&1 || true
                        ${codex} plugin marketplace add ${root} --json >/dev/null
                      ''
                    ) cfg.codex.marketplaces}
                    ${lib.concatMapStringsSep "\n" (
                      selector:
                      "CODEX_HOME=${lib.escapeShellArg codexHome} ${lib.getExe cfg.codex.package} plugin add ${lib.escapeShellArg selector} --json >/dev/null"
                    ) cfg.codex.plugins}
                  fi
                '';
              };
      };
  };
}
