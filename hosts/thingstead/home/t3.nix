{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  cfg = config.my.services.t3code;
  agents = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
  providerPackages = [
    agents.codex
    agents.claude-code
  ];
  package = pkgs.t3code.override { inherit providerPackages; };
  jsonFormat = pkgs.formats.json { };
  json5 = pkgs.python3Packages.toPythonApplication pkgs.python3Packages.json5;
  t3Home = "${config.xdg.configHome}/t3";
  userSettings = {
    enableProviderUpdateChecks = false;
    storageCleanup = {
      worktreeAfterDays = 90;
      worktreeOnMerge = true;
    };
    projectSettingsFolded = true;
    idleCompaction = "onReturn";
    providerInstances = {
      pi = {
        driver = "pi";
        displayName = "Pi";
        environment = [
          {
            name = "PI_CODING_AGENT_DIR";
            value = cfg.piAgentDir;
            sensitive = false;
          }
        ];
        enabled = true;
      };
      claudeAgent = {
        driver = "claudeAgent";
        enabled = true;
        config.homePath = cfg.claudeConfigDir;
      };
      codex = {
        driver = "codex";
        config.homePath = cfg.codexHome;
      };
    };
  };
in
{
  options.my.services.t3code = {
    enable = lib.mkEnableOption "T3 Code and its headless user service";
    port = lib.mkOption {
      type = lib.types.port;
      default = 3773;
      description = "Loopback port for the T3 Code HTTP/WebSocket server. Must be unique per user on a host; expose it with a system-level proxy such as Tailscale Serve.";
    };
    piAgentDir = lib.mkOption {
      type = lib.types.str;
      default = "${config.xdg.configHome}/pi/agent";
      description = "PI_CODING_AGENT_DIR used by the pi CLI and T3's Pi provider.";
    };
    claudeConfigDir = lib.mkOption {
      type = lib.types.str;
      default = "${config.xdg.configHome}/claude";
      description = "CLAUDE_CONFIG_DIR used by the claude CLI and T3's Claude provider.";
    };
    codexHome = lib.mkOption {
      type = lib.types.str;
      default = "${config.xdg.configHome}/codex";
      description = "CODEX_HOME used by the codex CLI and T3's Codex provider.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ package ] ++ providerPackages;
    programs.pi = {
      enable = lib.mkDefault true;
      package = lib.mkDefault agents.pi;
    };
    home.sessionVariables = {
      T3CODE_HOME = t3Home;
      PI_CODING_AGENT_DIR = cfg.piAgentDir;
      CLAUDE_CONFIG_DIR = cfg.claudeConfigDir;
      CODEX_HOME = cfg.codexHome;
    };

    home.activation.t3codeSettings = lib.hm.dag.entryAfter [ "linkGeneration" ] (
      lib.hm.generators.mkImpureConfigMerger {
        inherit pkgs;
        format = "json";
        empty = "{}";
        jqOperation = "$dynamic * $static";
        path = "${t3Home}/userdata/settings.json";
        mode = "600";
        staticSettings = jsonFormat.generate "t3code-user-settings" userSettings;
        reader = "${lib.getExe json5} --as-json";
      }
    );

    systemd.user.services.t3code = {
      Unit = {
        Description = "T3 Code headless server";
        After = [ "network.target" ];
      };
      Service = {
        WorkingDirectory = config.home.homeDirectory;
        ExecStart = "${lib.getExe' package "t3"} serve --port ${toString cfg.port}";
        Environment = [
          "T3CODE_HOME=${t3Home}"
          "PI_CODING_AGENT_DIR=${cfg.piAgentDir}"
          "CLAUDE_CONFIG_DIR=${cfg.claudeConfigDir}"
          "CODEX_HOME=${cfg.codexHome}"
        ];
        Restart = "on-failure";
        RestartSec = 5;
      };
      Install.WantedBy = [ "default.target" ];
    };
  };
}
