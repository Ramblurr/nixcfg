{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  hn = "quine";
in {
  imports = [
    ./hardware-configuration.nix
    ../../profiles/interactive.nix
    #../../profiles/addon-laptop.nix
    ../../profiles/addon-desktop.nix
    ../../profiles/addon-gaming.nix

    ../../mixins/impermanence.nix
    ../../mixins/cpu-ryzen.nix
    ../../mixins/gfx-nvidia.nix
    ../../mixins/gui-wayland.nix
    ../../mixins/gui-plasma.nix
    ../../mixins/gui-hyprland.nix
    ../../mixins/hyprland.nix
    ../../mixins/syncthing.nix
    ../../mixins/zfs.nix
    inputs.nixos-hardware.nixosModules.common-cpu-amd
    inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
    inputs.nixos-hardware.nixosModules.common-gpu-nvidia
  ];

  config = {
    nixpkgs.hostPlatform.system = "x86_64-linux";
    system.stateVersion = "23.05";
    environment.systemPackages = with pkgs; [
    ];

    nixcfg.common.hostColor = "yellow";

    #services.tailscale.useRoutingFeatures = "server";

    networking.hostName = hn;
    networking.dhcpcd.wait = "background";
    networking.dhcpcd.extraConfig = "noarp";

    boot = {
      #kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
      initrd = {
        availableKernelModules = ["aesni_intel" "cryptd"];

        luks.devices = {
          cryptkey = {
            device = "/dev/disk/by-label/cryptkey";
          };

          cryptswap = {
            device = "/dev/disk/by-label/cryptswap";
            keyFile = "/dev/mapper/cryptkey";
            keyFileSize = 64;
          };
        };

        postMountCommands = ''
          # Don't keep the cryptkey available all the time.
          cryptsetup close /dev/mapper/cryptkey
        '';

        postDeviceCommands = lib.mkAfter ''
          zfs rollback -r rpool/local/root@blank
        '';
      };
    };
    #users.mutableUsers = false;
    #users.users.root.initialHashedPassword = "...";

    fileSystems."/var/log".neededForBoot = true;
  };
}
