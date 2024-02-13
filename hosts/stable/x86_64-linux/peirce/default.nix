{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  hn = "peirce";
  machine-id = "990ad480d2bf4eacb5d0086ab46ea3f9";
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
    hostname = hn;
    clusterSettings = k3s-main;
    user = ramblurr;
    defaultSopsFile = defaultSopsFile;
  };
  modules.server.virtd-host = {
    enable = true;
    zfsStorage.enable = true;
  };
}
