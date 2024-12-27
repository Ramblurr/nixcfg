{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  hn = "debord";
  defaultSopsFile = ./secrets.sops.yaml;
  ramblurr = import ../ramblurr.nix {
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
    ../../config/secrets.nix
    ../../config/home-ops.nix
  ];
  system.stateVersion = "24.05";
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;
  repo.secretFiles.home-ops = ../../secrets/home-ops.nix;
  sops.defaultSopsFile = ./secrets.sops.yaml;
  modules.networking.default.hostName = hn;

  modules.vpn.tailscale.enable = true;
  modules.vpn.tailscale.useRoutingFeatures = "both";
  home-ops = {
    enable = true;
    user = ramblurr;
    containers.enable = false;
    hypervisor.enable = true;
  };
  myhm =
    { pkgs, ... }@hm:
    {
      #home.persistence."/persist${ramblurr.homeDirectory}" = {
      #  directories = [ { directory = "work"; } ];
      #};
    };
}
