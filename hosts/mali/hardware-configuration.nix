{
  lib,
  modulesPath,
  ...
}:
{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];
  fileSystems."/" = {
    device = "rpool2/encrypted/local/root";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "vfat";
    neededForBoot = true;
  };

  fileSystems."/nix" = {
    device = "rpool2/encrypted/local/nix";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/home" = {
    device = "rpool2/encrypted/local/home";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/persist" = {
    device = "rpool2/encrypted/safe/persist";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/persist/extra" = {
    device = "rpool2/encrypted/safe/persist/extra";
    fsType = "zfs";
    neededForBoot = true;
  };

  fileSystems."/persist/extra/atuin" = {
    device = "rpool2/encrypted/safe/persist/extra/atuin";
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
      checkJournalingFS = true;
      supportedFilesystems = [ "ext4" ];

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
          device = "/dev/disk/by-partlabel/cryptkey";
          keyFile = "/dev/disk/by-partlabel/usbbootkey";
          keyFileSize = 4096;
          keyFileTimeout = 10;
        };

        cryptswap = {
          device = "/dev/disk/by-label/cryptswap";
          keyFile = "/keyfile:/dev/mapper/cryptkey";
          keyFileSize = 64;
        };
      };

      systemd = {
        enable = true;

        mounts = [
          {
            what = "/dev/mapper/cryptkey";
            where = "/run/cryptkey";
            type = "ext4";
            options = "ro";
            requires = [ "systemd-cryptsetup@cryptkey.service" ];
            after = [ "systemd-cryptsetup@cryptkey.service" ];
            before = [ "zfs-import-rpool2.service" ];
            wantedBy = [ "initrd.target" ];
          }
        ];

        services.zfs-import-rpool2 = {
          requires = [
            "systemd-cryptsetup@cryptkey.service"
            "run-cryptkey.mount"
          ];
          after = [
            "systemd-cryptsetup@cryptkey.service"
            "run-cryptkey.mount"
          ];
        };
      };
    };
  };
}
