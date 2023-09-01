{
  config,
  lib,
  pkgs,
  ...
}: {
  sops.secrets.minio-root-credentials = {
    owner = "minio";
  };
  services.minio = {
    enable = true;
    dataDir = ["/mnt/tank2/services/minio"];
    package = pkgs.minio;
    rootCredentialsFile = config.sops.secrets.minio-root-credentials.path;
    listenAddress = "127.0.0.1:9000";
    consoleAddress = "127.0.0.1:8889";
  };

  environment.persistence."/persist" = {
    directories = [
      "/var/lib/minio/"
    ];
  };
}
