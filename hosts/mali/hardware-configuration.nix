{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:
{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];
  fileSystems."/" = {
    device = "rpool/encrypted/local/root";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "vfat";
    neededForBoot = true;
  };

  fileSystems."/nix" = {
    device = "rpool/encrypted/local/nix";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/home" = {
    device = "rpool/encrypted/local/home";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/persist" = {
    device = "rpool/encrypted/safe/persist";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/persist/extra" = {
    device = "rpool/encrypted/safe/persist/extra";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/persist/extra/atuin" = {
    device = "rpool/encrypted/safe/persist/extra/atuin";
    fsType = "zfs";
    neededForBoot = true;
  };
  swapDevices = [ { device = "/dev/mapper/cryptswap"; } ];

  boot = {
    zfs.devNodes = "/dev/disk/by-partuuid";
    loader = {
      efi = {
        canTouchEfiVariables = true;
      };
      systemd-boot = {
        enable = true;
        configurationLimit = 20;
      };
    };
    initrd = {
      availableKernelModules = [
        "aesni_intel"
        "ahci"
        "cryptd"
        "mpt3sas"
        "nvme"
        "sd_mod"
        "usbhid"
        "usb_storage"
        "xhci_pci"
      ];

      kernelModules = [ "usb_storage" ];

      luks.devices = {
        cryptkey = {
          device = "/dev/disk/by-label/cryptkey";
          keyFileSize = 4096;
          keyFile = "/dev/disk/by-partlabel/usbbootkey";
          fallbackToPassword = true;
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
        zfs rollback -r rpool/encrypted/local/root@blank && \
        zfs rollback -r rpool/encrypted/local/home@blank && \
        echo "rollback complete"
      '';
    };
  };
}
