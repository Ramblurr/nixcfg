{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  cfg = config.modules.dev.llms.t3;
  agents = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
  package = agents.t3code.override {
    providerPackages =
      lib.optional config.modules.dev.llms.codex.enable config.modules.dev.llms.codex.finalPackage
      ++ lib.optional config.modules.dev.llms.claude.enable config.modules.dev.llms.claude.finalPackage
      ++ lib.optional config.modules.dev.llms.pi.enable config.modules.dev.llms.pi.package;
  };
  jsonFormat = pkgs.formats.json { };
  json5 = pkgs.python3Packages.toPythonApplication pkgs.python3Packages.json5;
in
{
  options.modules.dev.llms.t3 = {
    enable = lib.mkEnableOption "T3 Code and its headless user service";
    settings = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "T3 settings merged with the managed provider defaults.";
    };
    desktop.enable = lib.mkEnableOption "the T3 Code desktop app";
    tailscaleServePort = lib.mkOption {
      type = lib.types.nullOr lib.types.port;
      default = null;
      description = "Tailscale Serve HTTPS port, or null to disable. The user needs Tailscale operator access.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.tailscaleServePort == null || config.services.tailscale.enable;
        message = "T3 Code Tailscale Serve requires services.tailscale.enable.";
      }
    ];

    myhm =
      hm:
      let
        t3Home = "${hm.config.xdg.configHome}/t3";

      in
      {
        home.packages = [
          package
        ]
        ++ lib.optional cfg.desktop.enable (agents.t3code-desktop.override { t3code = package; });
        home.sessionVariables.T3CODE_HOME = t3Home;

        xdg.desktopEntries.t3code = lib.mkIf cfg.desktop.enable {
          name = "T3 Code";
          comment = "Control surface for coding agents";
          exec = ''${pkgs.coreutils}/bin/env "T3CODE_HOME=${hm.config.xdg.configHome}/t3-desktop" ${package.desktop}/bin/t3code-desktop %U'';
          icon = "t3code";
          categories = [ "Development" ];
          settings.StartupWMClass = "t3code";
        };

        home.activation.t3codeSettings = hm.lib.hm.dag.entryAfter [ "linkGeneration" ] (
          hm.lib.hm.generators.mkImpureConfigMerger {
            inherit pkgs;
            format = "json";
            empty = "{}";
            jqOperation = "$dynamic * $static";
            path = "${t3Home}/userdata/settings.json";
            mode = "600";
            staticSettings = jsonFormat.generate "t3code-user-settings" cfg.settings;
            reader = "${lib.getExe json5} --as-json";
          }
        );

        systemd.user.services.t3code = {
          Unit = {
            Description = "T3 Code headless server";
            After = [ "network.target" ];
          };
          Service = {
            ExecStart =
              "${lib.getExe' package "t3"} serve"
              + lib.optionalString (
                cfg.tailscaleServePort != null
              ) " --tailscale-serve --tailscale-serve-port ${toString cfg.tailscaleServePort}";
            Environment = [
              "T3CODE_HOME=${t3Home}"
            ]
            ++
              lib.optional (cfg.tailscaleServePort != null)
                "PATH=/run/wrappers/bin:${
                  lib.makeBinPath [ config.services.tailscale.package ]
                }:${hm.config.home.profileDirectory}/bin:/run/current-system/sw/bin";
            Restart = "on-failure";
            RestartSec = 5;
          };
          Install.WantedBy = [ "default.target" ];
        };
      };
  };
}
