{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  defaultSopsFile = ./secrets.sops.yaml;
  inherit (config.networking) hostName;
in
{
  imports = [
    ./hardware.nix
    ./disk-config.nix
    ./prometheus.nix
    ./grafana
    ../../config
    ../../config/home-ops.nix
    ../../modules/site-net
  ];
  system.stateVersion = "24.05";
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;
  repo.secretFiles.home-ops = ../../secrets/home-ops.nix;
  sops.defaultSopsFile = ./secrets.sops.yaml;

  networking.firewall.logRefusedConnections = lib.mkForce false;
  modules.vpn.tailscale.enable = true;
  modules.vpn.tailscale.useRoutingFeatures = "both";
  boot.kernel.sysctl."net.ipv4.conf.all.forwarding" = true;
  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;
  security.rtkit.enable = true;

  networking.firewall.allowedUDPPorts = [
    4214
  ];
  networking.firewall.allowedTCPPorts = [
    4214
  ];
  environment.systemPackages = with pkgs; [
    alsa-utils
    pipewire
    wireplumber
    pulsemixer
    jless
    (pkgs.vlc.override {
      chromecastSupport = false;
      jackSupport = false;
      onlyLibVLC = false;
      skins2Support = false;
      waylandSupport = false;
      withQt5 = false;
    })
  ];
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
    jack.enable = false;
    wireplumber.enable = true;
    audio.enable = true;
  };
  home-ops = {
    enable = true;
    containers.enable = false;
    hypervisor.enable = true;
    ingress.enable = true;
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
      "10-lan1" = {
        matchConfig.MACAddress = config.repo.secrets.local.lan1.hwaddr;
        linkConfig.Name = "lan1";
      };
    };

    networks = {
      "10-lan1" =
        let

          hostConfig = config.site.hosts.${hostName};
          hostBridges = lib.naturalSort (
            lib.mori.keys (lib.mori.filter (_: iface: iface.type == "bridge") hostConfig.interfaces)
          );
          vlansForThisIface = (
            lib.mori.filter (
              bridgeName:
              (hostConfig.interfaces.${bridgeName}.parent != null)
              && (hostConfig.interfaces.${bridgeName}.parent == "lan1")
            ) hostBridges
          );
        in
        {
          matchConfig.Name = "lan1";
          networkConfig = {
            DHCPServer = false;
            VLAN = map (net: "vlan-${net}") vlansForThisIface;
            LinkLocalAddressing = false;
            LLDP = true;
            EmitLLDP = true;
            Description = "I am the 10gbe sfp+ link";
          };
          linkConfig = {
            MTUBytes = 9000;
            RequiredForOnline = "carrier";
          };
        };
    };
  };
  modules.server.virtd-host.net.prim.iface = "prim";
}
