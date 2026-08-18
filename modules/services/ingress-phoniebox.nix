{
  config,
  lib,
  ...
}:

let
  cfg = config.modules.services.ingress-phoniebox;
  homeDomain = config.repo.secrets.home-ops.homeDomain;
  phonieboxDomain = "phoniebox.${homeDomain}";
in
{
  options.modules.services.ingress-phoniebox.enable = lib.mkEnableOption "Phoniebox ingress";

  config = lib.mkIf cfg.enable {
    site.gatus.endpoints = [
      {
        name = "Phoniebox";
        group = "Media & Library";
        url = "https://${phonieboxDomain}/";
        conditions = [ "[STATUS] == any(200, 406)" ];
      }
    ];

    modules.services.caddy.routes.phoniebox = {
      publicHost = phonieboxDomain;
      handlerConfig = ''
        handle_path /.fairybox-offline/* {
          header Cache-Control "public, max-age=3600"
          root * ${./ingress-phoniebox}
          file_server
        }
        route {
          intercept {
            @phoniebox_offline status 502 503 504
            handle_response @phoniebox_offline {
              rewrite * /index.html
              root * ${./ingress-phoniebox}
              file_server {
                status 503
              }
            }
          }
          reverse_proxy 10.9.6.26:80 {
            flush_interval -1
            transport http {
              dial_timeout 3s
              response_header_timeout 1h
            }
          }
        }
      '';
      errorHandlerConfig = ''
        @phoniebox_proxy_error {
          host ${phonieboxDomain}
          expression {http.error.status_code} in [502, 503, 504]
        }
        handle @phoniebox_proxy_error {
          rewrite * /index.html
          root * ${./ingress-phoniebox}
          file_server {
            status 503
          }
        }
      '';
    };
  };
}
