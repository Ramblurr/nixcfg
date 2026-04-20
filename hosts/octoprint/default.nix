{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  mjpegStreamerPort = 8080;
  ffmpeg = pkgs.rpi.ffmpeg_8-headless;
  rpicamApps = pkgs.rpi.rpicam-apps;
in
{
  imports = [
    inputs.nixos-raspberrypi.nixosModules.nixpkgs-rpi
    ./hardware.nix
    ./plugins.nix
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
    hardware.rpi.camera-rpi-v2.enable = true;
  };
  home.wifi.iot.enable = true;
  services.tailscale.enable = true;
  networking = {
    useDHCP = true;
    interfaces.eth0.useDHCP = true;
  };
  networking.firewall.allowedTCPPorts = [ mjpegStreamerPort ];
  users.users.octoprint.extraGroups = [
    "video"
  ];

  environment.systemPackages = [
    ffmpeg
    pkgs.v4l-utils # camera control
  ];

  services.octoprint = {
    enable = true;
    user = "octoprint";
    stateDir = "/var/lib/octoprint";
    port = 5000;
    openFirewall = true;
    plugins =
      plugins: with plugins; [
        camerasettings
        mqtt
        octoprint-homeassistant
        octoprint-cancelobject
        octoprint-costestimation
        octoprint-dashboard
        displaylayerprogress
        octoprint-excluderegion
        octoprint-filemanager
        octoprint-powerfailure
        octoprint-prettygcode
        octoprint-preheat
        octoprint-printtimegenius
        octoprint-prusaslicerthumbnails
        octoprint-printjobhistory
        octoprint-slicerestimator
        octoprint-spoolmanager
        #octoprint-tplinksmartplug
        octoprint-uicustomizer
      ];
    extraConfig = {
      webcam.ffmpeg = lib.getExe ffmpeg;
      server = {
        commands = {
          serverRestartCommand = "/run/wrappers/bin/sudo /run/current-system/sw/bin/systemctl restart octoprint";
          systemRestartCommand = "/run/wrappers/bin/sudo /run/current-system/sw/bin/systemctl reboot";
          systemShutdownCommand = "/run/wrappers/bin/sudo /run/current-system/sw/bin/systemctl poweroff";
        };
      };
      plugins = rec {
        _disabled = [

          "firmware_check"
          "softwareupdate"
        ];
      };
    };
  };
  systemd.services.octostream = {
    description = "MJPEG stream for OctoPrint";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    serviceConfig = {
      ExecStart = "${lib.getExe pkgs.python3} ${./mjpeg_server.py} ${toString mjpegStreamerPort} ${rpicamApps}/bin/rpicam-vid -t 0 --codec mjpeg --inline --width 1280 --height 720 --framerate 10 --vflip --hflip -n -o -";
      Restart = "always";
      RestartSec = "5s";
      SupplementaryGroups = [ "video" ];
      DynamicUser = true;
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
  services.udev.enable = true;
  services.udev.extraRules = ''
    # fixes "Could not open any dmaHeap device"
    # https://raspberrypi.stackexchange.com/a/141107
    SUBSYSTEM=="dma_heap", GROUP="video", MODE="0660"
  '';
  system.activationScripts.octostreamDmaHeapTrigger = ''
    if [ -d /sys/class/dma_heap ]; then
      ${pkgs.systemd}/bin/udevadm control --reload-rules || true
      ${pkgs.systemd}/bin/udevadm trigger --subsystem-match=dma_heap || true
    fi
  '';
}
