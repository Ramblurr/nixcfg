{
  workerHost = "quine";
  secretsDirectory = "/var/lib/immich-secrets";
  uid = 3024;
  gid = 3024;
  mediaLocation = "/var/lib/immich";
  mediaExport = "/mnt/tank2/services/immich-home";
  mediaDataset = "tank2/services/immich-home";
  machineLearning = {
    host = "peirce";
    port = 3443;
    localProxyPort = 3004;
  };
}
