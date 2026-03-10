{
  lib,

  ...
}:

let

  genGuestSecret = hostname: {
    "microvm-${hostname}-sops-key" = {
      sopsFile = ../../guests/${hostname}/secrets.sops.yaml;
      key = "ssh_host_ed25519_key";
      owner = "microvm";
      mode = "400";
    };
  };
  guests = [
    "dev1"
  ];
in
{
  modules.microvm-host = {
    enable = true;
    baseZfsDataset = "rpool/encrypted/safe/microvms";
  };

  sops.secrets = lib.mori.reduceAttrs genGuestSecret guests;
}
