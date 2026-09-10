{ config, lib, ... }:
let
  instance = import ./immich-home.nix;
  address = net: host: builtins.head config.site.net.${net}.hosts4.${host};
  apiAddress = address "svc" "immich-home";
  workerAddress = address "svc" config.networking.hostName;
in
{
  modules.services.immich-worker.enable = true;
  services.immich = {
    mediaLocation = instance.mediaLocation;
    secretsFile = "${instance.secretsDirectory}/environment";
    database.host = apiAddress;
    redis = {
      host = apiAddress;
      port = 6379;
    };
    environment.IMMICH_MACHINE_LEARNING_URL = lib.mkForce "http://${workerAddress}:3003";
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
