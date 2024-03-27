{ config, pkgs, lib, inputs, ... }:
let
  hn = "mill";
  machine-id = "964beb5f91f644d8bea9f74366d1556d";
  defaultSopsFile = ./secrets.sops.yaml;
  ramblurr = import ../../../ramblurr.nix { inherit config lib pkgs inputs; };
in {
  imports = [ ./hardware.nix ./disk-config.nix ];
  system.stateVersion = "23.11";
  environment.etc."machine-id".text = machine-id;
  repo.secretFiles.k3s-main = ../../../../secrets/k3s-main.nix;
  modules.profiles.k3s-node = {
    enable = true;
    hostname = hn;
    clusterSettings = config.repo.secrets.k3s-main;
    user = ramblurr;
    defaultSopsFile = defaultSopsFile;
  };
  modules.server.virtd-host = {
    enable = true;
    zfsStorage.enable = true;
  };
}
