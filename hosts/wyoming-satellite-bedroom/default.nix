{
  config,
  pkgs,
  lib,
  inputs,
  unstable,
  ...
}:
{
  imports = [
    inputs.nixos-raspberrypi.nixosModules.nixpkgs-rpi
    ./hardware.nix
    ../../config
  ];
  system.stateVersion = "25.11";
  environment.etc."machine-id".text = config.repo.secrets.local.machineId;
  sops.defaultSopsFile = ./secrets.sops.yaml;
  time.timeZone = "Europe/Berlin";

  modules = {
    shell = {
      htop.enable = true;
      tmux.enable = true;
      zsh.enable = true;
    };
    services = {
      sshd.enable = true;
    };
    editors.vim.enable = true;
    impermanence.enable = false;
    boot.zfs.enable = false;
    users.enable = true;
    users.userborn.enable = false;
    users.headless.enable = true;
    users.primaryUser.extraGroups = [ "wheel" ];
    hardware.fwupd.enable = false;
    hardware.udisks2.enable = false;
  };
  home.wifi.iot.enable = true;
  services.tailscale.enable = true;
  networking = {
    useDHCP = true;
    interfaces.eth0.useDHCP = true;
  };
  environment.systemPackages = [
    pkgs.pipewire
    pkgs.alsa-utils
  ];
  # this is a system wide pipewire config, users have to be in the pipewire group
  users.users.ramblurr.extraGroups = [ "pipewire" ];
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    audio.enable = true;
    systemWide = true;
    wireplumber.enable = true;
  };
  networking.firewall.allowedTCPPorts = [
    10700 # wyoming-satellite
  ];
  services.wyoming.satellite = {
    enable = true;
    user = "ramblurr";
    group = "pipewire";
    uri = "tcp://0.0.0.0:10700";
    name = "bedroom-announce-satellite";
    area = "bedroom";
    sound = {
      command = "pw-cat --playback --raw --rate=22050 --channels=1 --format=s16 -";
    };
    vad.enable = false;
  };

  systemd.services."wyoming-satellite" = {
    serviceConfig = {
      RestartSec = "1";
      Restart = "always";
      RuntimeMaxSec = "7200";
    };
    path = [
      pkgs.pipewire
      pkgs.pipewire
      pkgs.alsa-utils
    ];
  };
}
