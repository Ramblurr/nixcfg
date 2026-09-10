{ config, ... }:
let
  inherit (config.repo.secrets) home-ops;
  apiAddress = builtins.head config.site.net.svc.hosts4.immich-home;
in
{
  # The private wrapper supplies the evaluated, secret-aware guest configuration.
  microvm.vms.immich-home = {
    autostart = true;
    restartIfChanged = true;
  };
  modules.services.caddy.routes.immich-home = {
    publicHost = "photos.${home-ops.homeDomain}";
    upstream = "${apiAddress}:2283";
    directWan = false;
    webSockets = true;
    requestBodyMaxSize = null;
  };
}
