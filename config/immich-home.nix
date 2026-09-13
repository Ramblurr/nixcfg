{
  workerHost = "peirce";
  workerNetwork = "prim";
  secretsDirectory = "/var/lib/immich-secrets";
  uid = 3024;
  gid = 3024;
  mediaLocation = "/var/lib/immich";
  mediaExport = "/mnt/tank2/services/immich-home";
  mediaDataset = "tank2/services/immich-home";
  derivedDataset = "fast/services/immich-derived";
  derivedDirectories = [
    "thumbs"
    "encoded-video"
  ];
  machineLearning = {
    credentials = {
      ca = "op://home-ops-prod/Immich ML CA";
      server = "op://home-ops-prod/Immich ML Peirce";
      apiClient = "op://home-ops-prod/Immich ML API";
    };
    host = "peirce";
    rawPort = 3003;
    port = 3443;
    localProxyPort = 3004;
  };
}
