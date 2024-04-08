{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  hn = "peirce";
  machine-id = "990ad480d2bf4eacb5d0086ab46ea3f9";
  defaultSopsFile = ./secrets.sops.yaml;
  ramblurr = import ../../../ramblurr.nix {
    inherit
      config
      lib
      pkgs
      inputs
      ;
  };
in
{
  imports = [
    ./hardware.nix
    ./disk-config.nix
  ];
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
