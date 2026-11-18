{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.repo.secrets.local.signalT3Bridge) account groupId;
  thread = "f05290eb-95f1-4966-9c4e-a99e4a24eab1";
in
{
  myhm = hm: {
    home.packages = [ pkgs.signal-cli ];
    systemd.user.services.signal-t3-bridge = {
      Unit = {
        Description = "Forward Signal group messages to a T3 thread";
        After = [
          "network-online.target"
          "t3code.service"
        ];
        Wants = [ "network-online.target" ];
      };
      Service = {
        ExecStart = lib.escapeShellArgs [
          (lib.getExe pkgs.babashka)
          "${../../scripts/signal-t3-bridge.clj}"
          "--account"
          account
          "--group-id"
          groupId
          "--thread"
          thread
          "--signal-cli"
          (lib.getExe pkgs.signal-cli)
          "--t3"
          "${hm.config.home.profileDirectory}/bin/t3"
        ];
        Environment = [ "T3CODE_HOME=${hm.config.xdg.configHome}/t3" ];
        Restart = "always";
        RestartSec = 10;
      };
      Install.WantedBy = [ "default.target" ];
    };
  };
}
