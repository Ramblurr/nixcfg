{
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (config.networking) hostName;
in
{
  imports = [
    ./hardware.nix
    ./disk-config.nix
    ./networking.nix
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

  security.rtkit.enable = true;

  environment.systemPackages = with pkgs; [
    alsa-utils
    pipewire
    wireplumber
    pulsemixer
    jless
    linux-voice-assistant-unstable
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
  myhm = _: {
    #home.persistence."/persist${ramblurr.homeDirectory}" = {
    #  directories = [ { directory = "work"; } ];
    #};
  };

  # Merge in the site secrets
  inherit (config.repo.secrets.site) site;
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
          vlansForThisIface = lib.mori.filter (
            bridgeName:
            (hostConfig.interfaces.${bridgeName}.parent != null)
            && (hostConfig.interfaces.${bridgeName}.parent == "lan1")
          ) hostBridges;
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
  services.linux-voice-assistant = {
    enable = true;
    openFirewall = true;
    user = "ramblurr";
    group = "audio";
    name = "kitchen-announce-satellite";
  };
  #systemd.services."linux-voice-assistant" = {
  #  serviceConfig = {
  #    RestartSec = "1";
  #    Restart = "always";
  #    RuntimeMaxSec = "7200";
  #  };
  #  path = [
  #    pkgs.pipewire
  #  ];
  #};
}
