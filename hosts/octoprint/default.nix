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
  users.users.octoprint.extraGroups = [
    "video"
  ];

  environment.systemPackages = [
    pkgs.rpi.ffmpeg-headless
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
      webcam.ffmpeg = lib.getExe pkgs.rpi.ffmpeg-headless;
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
    serviceConfig = {
      ExecStart = "${unstable.mjpg-streamer}/bin/mjpg_streamer -i \"input_uvc.so -r 1280x720 -d /dev/video0 -f 60 -n\" -o \"output_http.so -p 8080 -w /usr/local/share/mjpg-streamer/www\"";
    };
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" ];
    requires = [ "network-online.target" ];
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
