{
  config,
  lib,
  ...
}:
let
  inherit (config.repo.secrets.global) domain email;
  homeDomain = domain.home;
  minioAllowedRemoteIPs = [
    "10.9.8.0/23"
    "10.9.10.0/23"
  ];
  minioProxy = ''
    reverse_proxy http://127.0.0.1:8999 {
      header_up X-NginX-Proxy "true"
      header_up X-Real-IP {http.request.remote.host}
      header_up X-Forwarded-Proto "https"
      flush_interval -1
      transport http {
        dial_timeout 300s
      }
    }
  '';
in
{
  networking.firewall.allowedUDPPorts =
    lib.optionals (config.modules.services.caddy-security.edge.enable)
      [ 443 ];

  sops.templates.caddy-security-env = {
    owner = "caddy";
    group = "caddy";
    mode = "0400";
    restartUnits = [ "caddy.service" ];
    content = "DESEC_API_TOKEN=${config.sops.placeholder.desec_api_token}";
  };

  modules.services.caddy-security = {
    enable = true;
    environmentFile = config.sops.templates.caddy-security-env.path;
    edge = {
      enable = true;
      certificateHosts = [
        "attic.mgmt.${homeDomain}"
        "attic.int.${homeDomain}"
        "nix-cache.int.${homeDomain}"
        "s3.data.${homeDomain}"
        "*.s3.data.${homeDomain}"
        "minio.data.${homeDomain}"
        "*.s3.mgmt.${homeDomain}"
        "minio.mgmt.${homeDomain}"
        "s3.mgmt.${homeDomain}"
      ];
      protocols = [
        "h1"
        "h2"
        "h3"
      ];
      redirectPort = 80;
      redirectStatus = 301;
      acmeEmail = email.acme;
    };
  };

  modules.services.caddy.routes = {
    attic = {
      publicHost = "attic.mgmt.${homeDomain}";
      aliases = [ "attic.int.${homeDomain}" ];
      upstream = "http://127.0.0.1:57000";
    };
    minio-console = {
      publicHost = "minio.data.${homeDomain}";
      aliases = [ "minio.mgmt.${homeDomain}" ];
      allowedRemoteIPs = minioAllowedRemoteIPs;
      handlerConfig = ''
        handle_path /minio/ui/* {
          ${minioProxy}
        }
        handle {
          ${minioProxy}
        }
      '';
    };
    ncps = {
      publicHost = "nix-cache.int.${homeDomain}";
      upstream = "http://${config.services.ncps.server.addr}";
    };
    s3 = {
      publicHost = "s3.data.${homeDomain}";
      aliases = [ "s3.mgmt.${homeDomain}" ];
      upstream = "http://127.0.0.1:9000";
      allowedRemoteIPs = minioAllowedRemoteIPs;
      requestHeaders = {
        X-Real-IP = "{http.request.remote.host}";
        X-Forwarded-Proto = "https";
      };
      dialTimeout = "300s";
      flushInterval = "-1";
    };
  };
}
