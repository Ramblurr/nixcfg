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
    users.primaryUser.extraGroups = [
      "wheel"
    ];
    #users.primaryUser.password.enable = false;
    hardware = {
      fwupd.enable = false;
      udisks2.enable = false;
      rpi.type = "rpi4";
      rpi.hifiberry-dac-plus.enable = true;
    };
  };
  home.wifi.iot.enable = true;
  services.tailscale.enable = true;
  networking = {
    useDHCP = true;
    interfaces.eth0.useDHCP = true;
  };

  services.wyoming.satellite = {
    enable = true;
    user = "ramblurr";
    group = "audio";
    uri = "tcp://0.0.0.0:10700";
    name = "bedroom-announce-satellite";
    area = "bedroom";
    sound = {
      command = "pw-cat --playback --raw --rate=22050 --channels=1 --format=s16 -";
    };
    vad.enable = false;
  };
  systemd.services."wyoming-satellite".path = [
    pkgs.pipewire
    pkgs.alsa-utils
  ];

  environment.systemPackages = [
    pkgs.pipewire
    pkgs.alsa-utils
  ];
  # use system wide pipewire
  users.users.ramblurr.extraGroups = [ "pipewire" ];
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    audio.enable = true;
    systemWide = true;
    wireplumber.enable = true;
  };
}
