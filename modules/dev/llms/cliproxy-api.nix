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
  };

  config = lib.mkIf cfg.enable {
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
