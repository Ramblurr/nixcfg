{
  config,
  pkgs,
  ...
}:
let
  homeDomain = config.repo.secrets.global.domain.home;
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
  environment.systemPackages = with pkgs; [ minio-client ];
  sops.secrets.minio-root-credentials = {
    owner = "minio";
  };
  services.minio = {
    enable = true;
    dataDir = [ "/mnt/tank2/services/minio" ];
    package = pkgs.minio;
    rootCredentialsFile = config.sops.secrets.minio-root-credentials.path;
    listenAddress = "127.0.0.1:9000";
    consoleAddress = "127.0.0.1:8999";
  };

  environment.persistence."/persist" = {
    directories = [ "/var/lib/minio/" ];
  };
  modules.services.caddy.routes.minio-console = {
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
  modules.services.caddy.routes.s3 = {
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
}
