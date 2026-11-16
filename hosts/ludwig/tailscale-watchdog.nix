{
  config,
  lib,
  pkgs,
  ...
}:
#
# ludwig is remote; its own Tailscale node is one of its two ways in. Restart
# tailscaled while it is not connected. If the node ever needs a new login,
# place a tagged auth key at authKeyFile.
let
  authKeyFile = config.services.tailscale.authKeyFile;
in
{
  systemd.services.tailscaled-autoconnect.unitConfig.ConditionPathExists = lib.mkIf (
    authKeyFile != null
  ) authKeyFile;
  # `tailscale up` refuses an auth key while older prefs are set; extraSetFlags
  # reapplies ours afterwards.
  services.tailscale.extraUpFlags = [ "--reset" ];

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
        echo "tailscale needs a login; retrying the staged auth key"
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
