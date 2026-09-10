{
  config,
  lib,
  modulesPath,
  ...
}:
{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  boot = {
    zfs.devNodes = lib.mkForce "/dev/disk/by-partuuid";
    loader = {
      efi.canTouchEfiVariables = false;
      # GRUB copies kernels and boot configuration to both ESPs.
      grub = {
        enable = true;
        efiSupport = true;
        efiInstallAsRemovable = true;
        copyKernels = true;
        configurationLimit = 20;
        mirroredBoots = [
          {
            path = "/boot/crucial";
            devices = [ "nodev" ];
          }
          {
            path = "/boot/samsung";
            devices = [ "nodev" ];
          }
        ];
      };
    };
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
    initrd = {
      availableKernelModules = [
        "xhci_pci"
        "ahci"
        "usbhid"
        "usb_storage"
        "sd_mod"
      ];
      kernelModules = [ ];
      systemd.enable = false;
      # ZFS imports in postResumeCommands; prepare its key beforehand.
      postDeviceCommands = lib.mkBefore ''
        (
          umask 077
          for disk in crucial samsung; do
            keypart=/dev/disk/by-partlabel/peirce-cryptkey-$disk
            if [ -b "$keypart" ] && head -c 65 "$keypart" > /run/peirce-zfs.key; then
              break
            fi
          done
        )
      '';
      postResumeCommands = lib.mkAfter ''
        zfs rollback -r rpool/encrypted/local/root@blank && \
        zfs rollback -r rpool/encrypted/vms@blank && \
        echo "rollback complete"
      '';
    };
  };
}
