{ config, ... }:
let
  apiAddress = builtins.head config.site.net.svc.hosts4.immich-home;
in
{
  # The private wrapper supplies the evaluated guest and ingress host name.
  microvm.vms.immich-home = {
    autostart = true;
    restartIfChanged = true;
  };
  modules.services.caddy.routes.immich-home = {
    upstream = "${apiAddress}:2283";
    directWan = false;
    webSockets = true;
    requestBodyMaxSize = null;
  };
}
