{
  config,
  lib,
  pkgs,
  ...
}:
#
# ludwig is remote and reachable only through its own Tailscale node, on both
# the installer and the installed host. Restart tailscaled while it is not
# connected, as the TrueNAS cron check did for the jail. An auth key, when one
# was staged, is used only if the reused node state is rejected.
let
  authKeyFile = config.services.tailscale.authKeyFile;
in
{
  systemd.services.tailscaled-autoconnect.unitConfig.ConditionPathExists = lib.mkIf (
    authKeyFile != null
  ) authKeyFile;

  systemd.services.tailscale-watchdog = {
    description = "Restart tailscaled while it is not connected";
    after = [ "tailscaled.service" ];
    serviceConfig.Type = "oneshot";
    path = [
      config.services.tailscale.package
      config.systemd.package
      pkgs.jq
    ];
    script = ''
      state=$(tailscale status --json 2>/dev/null | jq -r .BackendState) || state=unknown
      case "$state" in
      Running) exit 0 ;;
      NeedsLogin)
        echo "tailscale needs a login; retrying the staged auth key if present"
        systemctl restart tailscaled-autoconnect.service || true
        ;;
      *)
        echo "tailscale backend state '$state'; restarting tailscaled"
        systemctl restart tailscaled.service
        ;;
      esac
    '';
  };
  systemd.timers.tailscale-watchdog = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "5min";
      OnUnitActiveSec = "5min";
    };
  };
}
