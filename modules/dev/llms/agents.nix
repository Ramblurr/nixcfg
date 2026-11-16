{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.dev.llms;
  extraFiles = lib.mkOption {
    type = lib.types.listOf lib.types.path;
    default = [ ];
    description = "Markdown files appended to the agent instructions, in order.";
  };
  instructionOptions = label: filename: {
    inherit extraFiles;
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Manage ${filename}.";
    };
    preamble = lib.mkOption {
      type = lib.types.lines;
      default = "# ${label} Defaults\n\nThese instructions apply to every ${label} session for this user.";
      description = "Text rendered at the start of ${filename}.";
    };
    extraText = lib.mkOption {
      type = lib.types.lines;
      default = "";
      description = "Additional text appended to ${filename}.";
    };
  };
  toolOptions = {
    extraArgs = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Host-declared arguments passed by the client launcher.";
    };
    enable = lib.mkOption {
      type = lib.types.bool;
      default = cfg.enable;
      description = "Enable this agent on this host.";
    };
    removeSettings = lib.mkOption {
      type = lib.types.listOf (lib.types.listOf lib.types.str);
      default = [ ];
      description = "Host-declared nested setting paths removed during mutable configuration merging.";
    };
    settings = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "Host-declared tool-native settings. Never include secret values.";
    };
  };
in
{
  options.modules.dev.llms = {
    agents = {
      inherit extraFiles;
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Manage global instruction files for enabled agents.";
      };
      preamble = lib.mkOption {
        type = lib.types.lines;
        default = "";
        description = "Common preamble rendered before each tool's instructions.";
      };
      extraText = lib.mkOption {
        type = lib.types.lines;
        default = "";
        description = "Shared instruction text supplied by the consuming NixOS configuration.";
      };
    };
    proxy = {
      removeLegacyCredentials = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Remove inline credentials from live settings after existing client processes have been restarted onto the credential-aware launchers.";
      };
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Route agents through CLIProxyAPI.";
      };
      url = lib.mkOption {
        type = lib.types.str;
        apply = lib.removeSuffix "/";
        default = "";
        description = "Proxy root URL, without /v1. Override for other proxy hosts.";
      };
      credential = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = "1Password reference for the proxy client key.";
      };
    };
    skills = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Install the packaged local skills for enabled agents.";
      };
      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.ol-skills;
        description = "Shared skills source and tool plugin outputs.";
      };
    };
    pi = toolOptions // {
      agents = instructionOptions "Pi" "Pi AGENTS.md";
      models = lib.mkOption {
        type = lib.types.attrs;
        default = { };
        description = "Pi models.json overrides shared by the pi and pi-t3 instances.";
      };
    };
    claude = toolOptions // {
      agents = instructionOptions "Claude" "~/.config/claude/CLAUDE.md";
      plugins = lib.mkOption {
        type = lib.types.attrsOf (
          lib.types.oneOf [
            lib.types.path
            lib.types.str
            lib.types.package
          ]
        );
        default = { };
        description = "Additional personal Claude plugin directories keyed by installation name.";
      };
    };
    codex = toolOptions // {
      agents = instructionOptions "Codex" "~/.config/codex/AGENTS.md";
      marketplaces = lib.mkOption {
        type = lib.types.listOf (
          lib.types.oneOf [
            lib.types.path
            lib.types.str
            lib.types.package
          ]
        );
        default = [ ];
        description = "Additional local marketplace directories registered through the Codex CLI.";
      };
      plugins = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Additional plugin@marketplace selectors installed through the Codex CLI.";
      };
    };
    nono.enable = lib.mkOption {
      type = lib.types.bool;
      default = cfg.enable;
      description = "Install nono profiles and ECA wrapper.";
    };
  };
}
