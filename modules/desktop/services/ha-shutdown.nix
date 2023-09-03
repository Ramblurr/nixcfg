{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.services.ha-shutdown;
  shutdownScript = pkgs.writeScript "ha-shutdown.py" (builtins.readFile ./shutdown.py);
  username = config.modules.users.primaryUser.username;
in {
  options.modules.desktop.services.ha-shutdown = {
    enable = mkEnableOption "ha-shutdown";
    environmentFile = mkOption {
      description = "The full path to a file that contains the secret environment variables needed for the shutdown service";
      type = with types; nullOr str;
      default = null;
    };
    listenPort = mkOption {
      type = types.int;
      default = 5001;
    };
    timeout = mkOption {
      type = types.int;
      default = 60000;
      description = "The number of milliseconds to wait for a response from Home Assistant before shutting down";
    };
  };
  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [
      cfg.listenPort
    ];
    sops.secrets.HA_SHUTDOWN_TOKEN = {
      owner = username;
      mode = "0400";
    };
    systemd.user.services.ha-shutdown = {
      description = "HA Shutdown Service";
      wantedBy = ["default.target"];
      after = ["network.target" "network-online.target" "sops-nix.service"];
      path = with pkgs; [python3 dunst systemd];
      serviceConfig = {
        EnvironmentFile = config.sops.secrets.HA_SHUTDOWN_TOKEN.path;
        ExecStart = "${pkgs.python3}/bin/python -u ${shutdownScript} --timeout ${toString cfg.timeout} --port ${toString cfg.listenPort} ";
        Restart = "always";
        RestartSec = "10s";
        StandardError = "journal";
        StandardOutput = "journal";
      };
    };
  };
}
