{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  cfg = config.modules.dev.llms.cliproxy-api;
in
{
  options.modules.dev.llms.cliproxy-api = {
    enable = lib.mkEnableOption "CLIProxyAPI and its user service";
    package = lib.mkOption {
      type = lib.types.package;
      default = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.cli-proxy-api;
      defaultText = lib.literalExpression "inputs.llm-agents.packages.\${system}.cli-proxy-api";
      description = "The CLIProxyAPI package.";
    };
    port = lib.mkOption {
      type = lib.types.port;
      default = 8317;
      description = "Loopback port the proxy listens on. Must match `port` in the unmanaged config.yaml.";
    };
    tailscaleServePort = lib.mkOption {
      type = lib.types.nullOr lib.types.port;
      default = null;
      description = "Tailscale Serve HTTPS port, or null to disable.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.tailscaleServePort == null || config.services.tailscale.enable;
        message = "CLIProxyAPI Tailscale Serve requires services.tailscale.enable.";
      }
    ];

    # Foreground Serve owns only this port and withdraws it on service stop.
    systemd.services.cliproxy-api-tailscale-serve = lib.mkIf (cfg.tailscaleServePort != null) {
      description = "CLIProxyAPI Tailscale HTTPS listener";
      wantedBy = [ "multi-user.target" ];
      after = [ "tailscaled.service" ];
      requires = [ "tailscaled.service" ];
      serviceConfig = {
        ExecStart = "${lib.getExe config.services.tailscale.package} serve --https=${toString cfg.tailscaleServePort} http://127.0.0.1:${toString cfg.port}";
        Restart = "on-failure";
        RestartSec = 5;
      };
    };

    myhm =
      hm:
      let
        configFile = "${hm.config.xdg.configHome}/cliproxyapi/config.yaml";
      in
      {
        home.packages = [ cfg.package ];

        systemd.user.services.cliproxy-api = {
          Unit = {
            Description = "CLIProxyAPI";
            After = [ "network.target" ];
            ConditionPathExists = configFile;
          };
          Service = {
            ExecStart = "${lib.getExe cfg.package} --config ${configFile}";
            Restart = "on-failure";
            RestartSec = 5;
          };
          Install.WantedBy = [ "default.target" ];
        };
      };
  };
}
