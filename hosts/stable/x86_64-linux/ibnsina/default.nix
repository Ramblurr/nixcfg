{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  hn = "ibnsina";
  machine-id = "0cbc5a0908b84c809b0d02f64837ec05";
  defaultSopsFile = ./secrets.sops.yaml;
  ramblurr = import ../../../ramblurr.nix {inherit config lib pkgs inputs;};
  k3s-main = builtins.fromJSON (builtins.readFile ../../../../secrets/k3s-main.secrets);
in {
  imports = [
    ./hardware.nix
    ./disk-config.nix
  ];
  system.stateVersion = "23.11";
  environment.etc."machine-id".text = machine-id;

  modules.profiles.k3s-node = {
    enable = true;
    server.enable = true;
    hostname = "ibnsina";
    clusterSettings = k3s-main;
    user = ramblurr;
    defaultSopsFile = defaultSopsFile;
  };
  modules.server.virtd-host = {
    enable = true;
    zfsStorage.enable = true;
  };
}
