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
    ../../modules/loginctl-linger.nix
    ./hardware-configuration.nix
    ../../profiles/interactive.nix
    ../../profiles/addon-gaming.nix
    ../../profiles/gui-wayland.nix
    ../../profiles/gui-plasma.nix
    ../../profiles/gui-hyprland.nix

    ../../mixins/impermanence.nix
    ../../mixins/cpu-ryzen.nix
    ../../mixins/gfx-nvidia.nix
    ../../mixins/hyprland.nix
    ../../mixins/syncthing.nix
    ../../mixins/zfs.nix
    ../../mixins/1password.nix
    ../../mixins/emacs.nix
    inputs.nixos-hardware.nixosModules.common-cpu-amd
    inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
  ];

  config = {
    nixpkgs.hostPlatform.system = "x86_64-linux";
    system.stateVersion = "23.05";
    environment.systemPackages = with pkgs; [
    ];

    nixcfg.common.hostColor = "yellow";
    sops.age.sshKeyPaths = [ "/persist/etc/ssh/ssh_host_ed25519_key" ];

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
