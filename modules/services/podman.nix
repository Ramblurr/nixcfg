{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.services.podman;

  podmanWaitForDns = pkgs.writeShellScript "podman-wait-for-dns" ''
    until ${pkgs.glibc.getent}/bin/getent ahostsv4 registry-1.docker.io >/dev/null 2>&1; do
      ${pkgs.coreutils}/bin/sleep 0.5
    done
  '';
in
{
  options.modules.services.podman = {
    enable = lib.mkEnableOption "";
  };
  config = lib.mkIf cfg.enable {
    virtualisation = {
      containers.enable = true;
      oci-containers.backend = "podman";
      podman = {
        enable = true;
        autoPrune.enable = true;
        autoPrune.dates = "weekly";
        autoPrune.flags = [ "--all" ];
      };
    };
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

    # Podman's rootless Quadlets already wait for this user unit to observe the
    # system network-online target. On these hosts, DNS is provided by the local
    # dnsdist instance and can remain unavailable while its upstream health
    # checks recover. Keep the unit activating until external DNS works so
    # containers do not exhaust their image-pull retries during boot.
    systemd.user.services.podman-user-wait-network-online = {
      serviceConfig = {
        ExecStartPost = podmanWaitForDns;
        TimeoutStartSec = "180s";
      };
    };
    systemd.user.services.podman-image-prune = {
      Unit.Description = "Prune unused rootless podman images";
      Service = {
        Type = "oneshot";
        ExecStart = "${pkgs.podman}/bin/podman image prune --all --force";
      };
    };
    systemd.user.timers.podman-image-prune = {
      Unit.Description = "Weekly rootless podman image prune";
      Timer = {
        OnCalendar = "weekly";
        Persistent = true; # run after missed time
        RandomizedDelaySec = "2h";
      };
      Install.WantedBy = [ "timers.target" ];
    };
  };
}
