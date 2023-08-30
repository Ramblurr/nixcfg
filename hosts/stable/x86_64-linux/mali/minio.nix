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
  };
  networking.firewall.allowedTCPPorts = [9000 9001];

  environment.persistence."/persist" = {
    directories = [
      "/var/lib/minio/"
    ];
  };
}
