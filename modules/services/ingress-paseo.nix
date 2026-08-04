{
  config,
  lib,
  ...
}:

let
  cfg = config.modules.services.ingress-paseo;
  homeDomain = config.repo.secrets.global.domain.home;
in
{
  options.modules.services.ingress-paseo.enable = lib.mkEnableOption "Paseo ingress";

  config = lib.mkIf cfg.enable {
    modules.services.ingress.virtualHosts."paseo.${homeDomain}" = {
      acmeHost = homeDomain;
      upstream = "http://quine.prim.${homeDomain}:6767";
      upstreamExtraConfig = ''
        client_max_body_size 100m;
        proxy_connect_timeout 120s;

        # Paseo uses long-lived terminal and WebSocket streams.
        proxy_buffering off;
        proxy_read_timeout 3600s;
        proxy_send_timeout 3600s;

        # Preserve Paseo's public origin and HTTPS scheme for WebSocket URLs.
        proxy_set_header Host $host;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      '';
    };
  };
}
