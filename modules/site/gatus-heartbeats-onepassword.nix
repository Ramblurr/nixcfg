{ config, lib, ... }:
let
  providerEnabled = config.modules.services.onepassword-systemd-credentials.enable;
  heartbeats = lib.filterAttrs (_: heartbeat: heartbeat.service != null) config.site.gatus.heartbeats;
in
{
  config = lib.mkMerge [
    (lib.mkIf providerEnabled {
      site.gatus.heartbeatToken.available = true;
    })
    (lib.mkIf (providerEnabled && heartbeats != { }) {
      modules.services.onepassword-systemd-credentials.consumers = lib.mapAttrs' (
        _: heartbeat:
        lib.nameValuePair heartbeat.service {
          gatus-token = config.site.gatus.heartbeatToken.onepasswordReference;
        }
      ) heartbeats;
    })
  ];
}
