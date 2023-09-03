{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.services.hacompanion;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
  tomlFormat = pkgs.formats.toml {};
in {
  options.modules.desktop.services.hacompanion = {
    enable = mkEnableOption "hacompanion";
    package = mkOption {
      type = types.package;
      default = pkgs.my.hacompanion;
    };

    environmentFile = mkOption {
      description = "The full path to a file that contains the secret environment variables to register hacomapnion with Home Assistant";
      type = with types; nullOr str;
      default = null;
    };
    configFile = mkOption {
      description = "The config path that hacompanion uses";
      type = types.path;
      default = pkgs.writeText "hacompanion-config" (toTOML cfg.settings);
    };
    settings = mkOption {
      type = tomlFormat.type;
      default = {};
    };
    unitAfter = mkOption {
      type = types.listOf types.str;
      default = [];
    };
    listenPort = mkOption {
      type = types.int;
    };
  };
  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [
      cfg.listenPort
    ];
    systemd.user.services.hacompanion = {
      after = ["network.target" "network-online.target" "graphical-session.target"] ++ cfg.unitAfter;
      requires = ["graphical-session.target"];
      partOf = ["graphical-session.target"];
      wantedBy = ["graphical-session.target"];
      description = "Home Assistant Desktop Companion";
      documentation = ["https://github.com/tobias-kuendig/hacompanion"];
      path = [cfg.package pkgs.coreutils];
      serviceConfig = {
        Type = "simple";
        Restart = "on-failure";
        RestartSec = "5s";
        EnvironmentFile = cfg.environmentFile;
        ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p %E/hacompanion";
        ExecStart = "${cfg.package}/bin/hacompanion -config=${cfg.configFile}";
      };
    };
  };
}
