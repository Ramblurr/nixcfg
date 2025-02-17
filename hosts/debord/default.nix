{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  defaultSopsFile = ./secrets.sops.yaml;
in
{
  imports = [
    ./hardware.nix
    ./disk-config.nix
    ../../config
    ../../config/home-ops.nix
    ../../modules/site-net
  ];
  system.stateVersion = "24.05";
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;
  repo.secretFiles.home-ops = ../../secrets/home-ops.nix;
  sops.defaultSopsFile = ./secrets.sops.yaml;

  networking.firewall.logRefusedConnections = lib.mkForce true;
  modules.vpn.tailscale.enable = true;
  modules.vpn.tailscale.useRoutingFeatures = "both";
  home-ops = {
    enable = true;
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

  # Merge in the site secrets
  site = config.repo.secrets.site.site;
  systemd.network = {
    links = {
      "10-lan0" = {
        matchConfig.MACAddress = config.repo.secrets.site.site.hosts.debord.interfaces.lan0.hwaddr;
        linkConfig.Name = "lan0";
      };
      #"10-lan1" = {
      #  matchConfig.MACAddress = config.repo.secrets.site.site.hosts.debord.interfaces.lan1.hwaddr;
      #  linkConfig.Name = "lan1";
      #};
    };
  };
  modules.server.virtd-host.net.brprim4.iface = "prim";
}
