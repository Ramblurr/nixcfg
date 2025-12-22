{
  config,
  lib,
  ...
}:
let

  guests = [
  ];

  genGuestSecret = hostname: {
    "microvm-${hostname}-sops-key" = {
      sopsFile = ../../guests/${hostname}/secrets.sops.yaml;
      key = "ssh_host_ed25519_key";
      owner = "microvm";
      mode = "400";
    };
  };
in
lib.mkMerge [
  {
    sops.secrets = lib.mori.reduceAttrs genGuestSecret guests;
  }
]
