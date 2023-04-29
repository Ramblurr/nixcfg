{
  config,
  lib,
  pkgs,
  ...
}: {
  environment.systemPackages = with pkgs; [
    microsocks
  ];
  systemd.services = {
    "mullvad-exclusion-init" = {
      enable = true;
      description = "sets up mullvad exclusion cgroup";
      after = ["network.target"];
      requires = ["network.target"];
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
    "microsocks" = {
      enable = true;
      description = "a tiny socks server";
      after = ["mullvad-exclusion-init.service"];
      requires = ["mullvad-exclusion-init.service"];
      restartIfChanged = true;
      serviceConfig = {
        Type = "simple";
        User = "ramblurr";
        KillMode = "process";
        Restart = "on-failure";
        RestartSec = "10s";
        ExecStart = "${pkgs.mullvad-vpn}/bin/mullvad-exclude ${pkgs.microsocks}/bin/microsocks -i 127.0.0.1 -p 1081";
      };
      path = [pkgs.microsocks];
    };
  };
}
