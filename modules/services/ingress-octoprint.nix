{ config, lib, ... }:
let
  cfg = config.modules.services.ingress-octoprint;
  homeDomain = config.repo.secrets.home-ops.homeDomain;
in
{
  options.modules.services.ingress-octoprint.enable = lib.mkEnableOption "OctoPrint Caddy route";

  config = lib.mkIf cfg.enable {
    site.gatus.endpoints = [
      {
        name = "OctoPrint";
        group = config.site.gatus.groups.home;
        url = "https://octoprint.${homeDomain}/online.txt";
      }
    ];

    modules.services.caddy.routes.octoprint = {
      publicHost = "octoprint.${homeDomain}";
      handlerConfig = ''
        handle_path /webcam/* {
          reverse_proxy 10.8.50.52:8080 {
            flush_interval -1
            header_down X-Accel-Buffering "no"
            transport http {
              read_timeout 24h
            }
          }
        }
        handle {
          reverse_proxy 10.8.50.52:5000
        }
      '';
    };
  };
}
