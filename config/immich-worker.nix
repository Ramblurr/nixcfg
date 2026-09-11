{ config, lib, ... }:
let
  instance = import ./immich-home.nix;
  address = net: host: builtins.head config.site.net.${net}.hosts4.${host};
  stateAddress = address instance.workerNetwork "immich-home";
  storageAddress = address instance.workerNetwork "mali";
in
{
  modules.services.immich-worker.enable = true;
  services.immich = {
    inherit (instance) mediaLocation;
    secretsFile = "${instance.secretsDirectory}/environment";
    database.host = stateAddress;
    redis = {
      host = stateAddress;
      port = 6379;
    };
    environment.IMMICH_MACHINE_LEARNING_URL = lib.mkForce "http://127.0.0.1:${toString instance.machineLearning.rawPort}";
    machine-learning.enable = false;
  };
  users.users.immich.uid = instance.uid;
  users.groups.immich.gid = instance.gid;
  fileSystems.${instance.mediaLocation} = {
    device = "${storageAddress}:${instance.mediaExport}";
    fsType = "nfs";
    options = [
      "addr=${storageAddress}"
      "vers=4.2"
      "hard"
      "_netdev"
      "x-systemd.mount-timeout=30s"
    ];
  };
  systemd.services.immich-server = {
    requires = [ "immich-machine-learning.service" ];
    after = [ "immich-machine-learning.service" ];
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
