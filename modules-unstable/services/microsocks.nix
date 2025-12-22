{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.microsocks;
  useMullvad = config.modules.vpn.mullvad.enable;
in
{
  options.modules.services.microsocks = {
    enable = lib.mkEnableOption "microsocks";
  };
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ microsocks ];
    services.microsocks = {
      enable = true;
      port = 1081;
      execWrapper = lib.optionalString useMullvad "${pkgs.mullvad-vpn}/bin/mullvad-exclude";
    };

    systemd.services.microsocks = lib.mkIf useMullvad {
      after = [ "mullvad-exclusion-init.service" ];
      wants = [ "mullvad-exclusion-init.service" ];
      requires = [ "mullvad-exclusion-init.service" ];
    };

    systemd.services.mullvad-exclusion-init = lib.mkIf useMullvad {
      enable = true;
      description = "sets up mullvad exclusion cgroup";
      after = [ "mullvad-daemon.service" ];
      wants = [ "mullvad-daemon.service" ];
      restartIfChanged = true;
      serviceConfig = {
        User = "root";
        ExecStart = "/bin/sh -c '${pkgs.coreutils}/bin/mkdir -p /sys/fs/cgroup/net_cls/mullvad-exclusions/ && ${pkgs.coreutils}/bin/touch /sys/fs/cgroup/net_cls/mullvad-exclusions/cgroup.procs && ${pkgs.coreutils}/bin/chmod 777 /sys/fs/cgroup/net_cls/mullvad-exclusions/cgroup.procs'";
        NotifyAccess = "all";
        Type = "oneshot";
        RemainAfterExit = "yes";
      };
      path = [ pkgs.coreutils ];
    };
  };
}
