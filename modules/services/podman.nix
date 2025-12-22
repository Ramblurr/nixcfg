{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.services.podman;
in
{
  options.modules.services.podman = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    environment.persistence."/persist" = {
      directories = [ "/var/lib/containers" ];
    };
    virtualisation.podman = {
      enable = true;
      autoPrune.enable = true;
      autoPrune.dates = "weekly";
      defaultNetwork.settings.dns_enabled = true;
      extraPackages = [ pkgs.zfs ];
    };
    environment.systemPackages = with pkgs; [
      docker-compose
      podman-tui
    ];
    systemd.services.podman-auto-update = {
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${pkgs.podman}/bin/podman auto-update";
        ExecStartPost = "${pkgs.podman}/bin/podman image prune -f";
      };
    };

    systemd.timers.podman-auto-update = {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "03:30";
        Persistent = true;
      };
    };
  };
}
