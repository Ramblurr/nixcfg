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
  cfg = config.modules.services.microsocks;
  username = config.modules.users.primaryUser.username;
  withImpermanence = config.modules.impermanence.enable;
  useMullvad = config.modules.vpn.mullvad.enable;
in {
  options.modules.services.microsocks = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      my.microsocks
    ];
    systemd.services = {
      mullvad-exclusion-init = mkIf useMullvad {
        enable = true;
        description = "sets up mullvad exclusion cgroup";
        after = ["mullvad-daemon.service"];
        wants = ["mullvad-daemon.service"];
        restartIfChanged = true;
        serviceConfig = {
          User = "root";
          ExecStart = "/bin/sh -c '${pkgs.coreutils}/bin/mkdir -p /sys/fs/cgroup/net_cls/mullvad-exclusions/ && ${pkgs.coreutils}/bin/touch /sys/fs/cgroup/net_cls/mullvad-exclusions/cgroup.procs && ${pkgs.coreutils}/bin/chmod 777 /sys/fs/cgroup/net_cls/mullvad-exclusions/cgroup.procs'";
          NotifyAccess = "all";
          Type = "oneshot";
          RemainAfterExit = "yes";
        };
        path = [pkgs.coreutils];
      };
      microsocks = {
        enable = true;
        description = "a tiny socks server";
        after = mkIf useMullvad ["mullvad-exclusion-init.service"];
        wants = mkIf useMullvad ["mullvad-exclusion-init.service"];
        requires = mkIf useMullvad ["mullvad-exclusion-init.service"];
        restartIfChanged = true;
        serviceConfig = {
          Type = "simple";
          User = "${username}";
          KillMode = "process";
          Restart = "on-failure";
          RestartSec = "10s";
          ExecStart =
            if useMullvad
            then "${pkgs.mullvad-vpn}/bin/mullvad-exclude ${pkgs.my.microsocks}/bin/microsocks -i 127.0.0.1 -p 1081"
            else "${pkgs.my.microsocks}/bin/microsocks -i 127.0.0.1 -p 1081";
        };
        path = [pkgs.my.microsocks];
      };
    };
  };
}
