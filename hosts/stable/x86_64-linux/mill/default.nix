{ config, pkgs, lib, inputs, ... }:
let
  hn = "mill";
  machine-id = "964beb5f91f644d8bea9f74366d1556d";
  defaultSopsFile = ./secrets.sops.yaml;
  ramblurr = import ../../../ramblurr.nix { inherit config lib pkgs inputs; };
  k3s-main = builtins.fromJSON (builtins.readFile ../../../../secrets/k3s-main.secrets);
in {
  imports = [ ./hardware.nix ./disk-config.nix ];
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
