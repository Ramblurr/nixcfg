{
  config,
  pkgs,
  lib,
  inputs,
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
    };
  };
  home.wifi.iot.enable = true;
  services.tailscale.enable = true;
  networking = {
    useDHCP = true;
    interfaces.eth0.useDHCP = true;
  };
  #services.mjpg-streamer = {
  #  enable = true;
  #  inputPlugin = "input_uvc.so --device ${camDevCont} --resolution 1280x720 --fps 30 -wb 4000 -bk 1 -ex 1000 -gain 255 -cagc auto -sh 100";
  #  outputPlugin = "output_http.so -w @www@ -n -p ${toString camPort}";
  #};
  services.octoprint = {
    enable = true;
    user = "octoprint";
    stateDir = "/var/lib/octoprint";
    port = 5000;
    openFirewall = true;
    plugins =
      plugins: with plugins; [
      ];
    extraConfig = {
      webcam.ffmpeg = lib.getExe pkgs.rpi.ffmpeg-headless;
      plugins = rec {
        _disabled = [ "softwareupdate" ];
      };
    };
  };

  security = {
    sudo.enable = true;
    sudo.wheelNeedsPassword = false;
    sudo.extraRules =
      let
        nopasswd = command: {
          inherit command;
          options = [
            "NOPASSWD"
            "SETENV"
          ];
        };
      in
      [
        {
          users = [ "octoprint" ];
          runAs = "root";
          commands = [ (nopasswd "/run/current-system/sw/bin/systemctl reboot") ];
        }
        {
          users = [ "octoprint" ];
          runAs = "root";
          commands = [ (nopasswd "/run/current-system/sw/bin/systemctl poweroff") ];
        }
        {
          users = [ "octoprint" ];
          runAs = "root";
          commands = [ (nopasswd "/run/current-system/sw/bin/systemctl restart octoprint") ];
        }
      ];
  };

  users.groups.dialout.members = [ "octoprint" ];
  users.groups.video.members = [ "octoprint" ];
  #boot.zfs.enabled = lib.mkDefault false;
}
