{ config, lib, ... }:
let
  heartbeats = lib.filterAttrs (_: heartbeat: heartbeat.service != null) config.site.gatus.heartbeats;
in
{
  config = lib.mkIf (heartbeats != { }) {
    modules.services.onepassword-systemd-credentials.consumers = lib.mapAttrs' (
      _: heartbeat:
      lib.nameValuePair heartbeat.service {
        gatus-token = config.site.gatus.heartbeatToken.onepasswordReference;
      }
    ) heartbeats;
  };
}
