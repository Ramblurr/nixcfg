{ config, lib, ... }:
let
  cfg = config.modules.services.ingress-home-assistant;
  homeDomain = config.repo.secrets.home-ops.homeDomain;
in
{
  options.modules.services.ingress-home-assistant.enable =
    lib.mkEnableOption "Home Assistant Caddy route";

  config = lib.mkIf cfg.enable {
    site.gatus.endpoints = [
      {
        name = "Home Assistant";
        group = config.site.gatus.groups.home;
        url = "https://home.${homeDomain}/";
      }
    ];

    modules.services.caddy.routes.home-assistant = {
      publicHost = "home.${homeDomain}";
      upstream = "http://10.9.4.25:8123";
    };
  };
}
