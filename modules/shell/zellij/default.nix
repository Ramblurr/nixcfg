{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
let
  cfg = config.modules.shell.zellij;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.shell.zellij = {
    enable = lib.mkEnableOption "";
    web = {
      enable = lib.mkEnableOption "";
      ip = lib.mkOption {
        type = lib.types.str;
        default = "127.0.0.1";
      };
      port = lib.mkOption {
        type = types.int;
        default = 8082;
      };
    };
  };
  config = mkIf cfg.enable {
    myhm = {
      home.file.".config/zellij/config.kdl".source = ./config.kdl;
      home.packages = with pkgs; [ zellij ];
      systemd.user.services.zellij-web = lib.mkIf cfg.web.enable {
        Unit = {
          Description = "Zellij Web (browser access to sessions)";
          After = [ "network-online.target" ];
          Wants = [ "network-online.target" ];
        };
        Service = {
          Type = "simple";
          ExecStart = ''${pkgs.zellij}/bin/zellij web --start --ip ${cfg.web.ip} --port ${toString cfg.web.port}'';
          Restart = "on-failure";
          RestartSec = 2;
        };
        Install.WantedBy = [ "default.target" ];
      };
    };

    systemd.services.tailscale-serve =
      lib.mkIf (cfg.web.enable && config.modules.vpn.tailscale.enable)
        {
          description = "zellij tailscale sserve";
          after = [
            "network-pre.target"
            "tailscale.service"
          ];
          wants = [
            "network-pre.target"
            "tailscale.service"
          ];
          wantedBy = [ "multi-user.target" ];
          serviceConfig.Type = "simple";
          serviceConfig.Restart = "on-failure";
          script = with pkgs; ''
            sleep 2
            ${tailscale}/bin/tailscale serve ${toString cfg.web.port};
          '';
        };
  };
}
