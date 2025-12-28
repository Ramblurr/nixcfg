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
    pkgs.pulsemixer
    pkgs.alsa-utils
    pkgs.linux-voice-assistant-unstable
  ];
  # this is a system wide pipewire config, users have to be in the pipewire group
  users.users.ramblurr.extraGroups = [ "pipewire" ];
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    audio.enable = true;
    pulse.enable = true;
    systemWide = true;
    wireplumber.enable = true;
  };
  services.linux-voice-assistant = {
    enable = true;
    openFirewall = true;
    user = "ramblurr";
    group = "pipewire";
    name = "bedroom-announce-satellite";
    audioOutputDevice = "pipewire";
  };
}
