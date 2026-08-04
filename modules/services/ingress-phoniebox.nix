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
    modules.services.ingress.virtualHosts.${phonieboxDomain} = {
      acmeHost = homeDomain;
      upstream = "http://10.9.6.26:80";
      upstreamExtraConfig = ''
        proxy_connect_timeout 3s;
        proxy_intercept_errors on;
        proxy_buffering off;
        proxy_cache off;
        proxy_read_timeout 1h;
        error_page 502 503 504 =503 /.fairybox-offline.html;
      '';
    };

    services.nginx.virtualHosts.${phonieboxDomain}.locations = {
      "= /.fairybox-offline.html" = {
        alias = "${./ingress-phoniebox}/index.html";
        extraConfig = ''
          internal;
        '';
      };
      "^~ /.fairybox-offline/" = {
        alias = "${./ingress-phoniebox}/";
        extraConfig = ''
          access_log off;
          expires 1h;
        '';
      };
    };
  };
}
