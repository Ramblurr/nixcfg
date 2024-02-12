{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

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

  fileSystems."/srv/data" = {
    device = "/dev/mapper/cryptxdata";
    fsType = "ext4";
    neededForBoot = true;
  };

  swapDevices = [
    {device = "/dev/mapper/cryptswap";}
  ];
  boot = {
    loader = {
      timeout = 3;
      efi = {
        canTouchEfiVariables = true;
      };
      systemd-boot = {
        enable = true;
        configurationLimit = 20;
      };
    };
    kernel.sysctl = {
      "fs.file-max" = 1048576;
      "fs.inotify.max_user_instances" = 256;
      "fs.inotify.max_user_watches" = 99999999;
    };

    initrd = {
      availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" "aesni_intel" "cryptd"];

      luks.devices = {
        cryptkey = {
          device = "/dev/disk/by-label/cryptkey";
        };

        cryptxdata = {
          device = "/dev/disk/by-label/cryptdata";
          keyFile = "/dev/mapper/cryptkey";
          keyFileSize = 64;
        };

        cryptswap = {
          device = "/dev/disk/by-label/cryptswap";
          keyFile = "/dev/mapper/cryptkey";
          keyFileSize = 64;
        };
      };

      #postMountCommands = ''
      #  # Don't keep the cryptkey available all the time.
      #  cryptsetup close /dev/mapper/cryptkey
      #'';

      postDeviceCommands = lib.mkAfter ''
        zfs rollback -r rpool/encrypted/local/root@blank && \
        zfs rollback -r rpool/encrypted/local/home@blank && \
        echo "rollback complete"
      '';
    };
  };
}
