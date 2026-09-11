{ config, lib, ... }:
let
  instance = import ./immich-home.nix;
  address = net: host: builtins.head config.site.net.${net}.hosts4.${host};
  apiAddress = address "svc" "immich-home";
  workerAddress = address "svc" config.networking.hostName;
  machineLearningAddress = address "prim" instance.machineLearning.host;
  machineLearningServerName = "immich-ml.${instance.machineLearning.host}.${config.site.net.prim.domainName}";
  credentialDirectory = "/run/credentials/immich-ml-client-proxy.service";
in
{
  modules.services.immich-worker.enable = true;
  modules.services.immich-ml-proxy = {
    enable = true;
    role = "client";
    port = instance.machineLearning.localProxyPort;
    upstreamAddress = machineLearningAddress;
    upstreamPort = instance.machineLearning.port;
    serverName = machineLearningServerName;
    allowedUser = config.services.immich.user;
    loadCredentials = false;
    credentials = {
      ca = "${credentialDirectory}/ca.pem";
      certificate = "${credentialDirectory}/certificate.pem";
      privateKey = "${credentialDirectory}/private-key.pem";
    };
  };
  services.immich = {
    inherit (instance) mediaLocation;
    secretsFile = "${instance.secretsDirectory}/environment";
    database.host = apiAddress;
    redis = {
      host = apiAddress;
      port = 6379;
    };
    environment.IMMICH_MACHINE_LEARNING_URL = lib.mkForce "http://127.0.0.1:${toString instance.machineLearning.localProxyPort}";
    machine-learning.environment.IMMICH_HOST = lib.mkForce workerAddress;
  };
  users.users.immich.uid = instance.uid;
  users.groups.immich.gid = instance.gid;
  fileSystems.${instance.mediaLocation} = {
    device = "${address "data" "mali"}:${instance.mediaExport}";
    fsType = "nfs";
    options = [
      "vers=4.2"
      "hard"
      "_netdev"
      "x-systemd.mount-timeout=30s"
    ];
  };
  networking.firewall.extraInputRules = ''
    ip saddr ${apiAddress} ip daddr ${workerAddress} tcp dport 3003 accept
  '';
  systemd.services.immich-server = {
    requires = [ "immich-ml-client-proxy.service" ];
    after = [ "immich-ml-client-proxy.service" ];
  };

  systemd.tmpfiles.rules = [ "d ${instance.secretsDirectory} 0700 root root -" ];
  environment.persistence."/persist".directories = lib.mkIf config.modules.impermanence.enable [
    {
      directory = instance.secretsDirectory;
      mode = "0700";
    }
    {
      directory = "/var/cache/immich";
      user = "immich";
      group = "immich";
      mode = "0700";
    }
  ];
}
