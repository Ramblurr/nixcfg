{
  config,
  lib,
  pkgs,
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
      efi = {
        canTouchEfiVariables = true;
      };
      systemd-boot = {
        enable = true;
        configurationLimit = 20;
      };
    };
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
    initrd = {
      availableKernelModules = [
        "aesni_intel"
        "ahci"
        "cryptd"
        "nvme"
        "rtsx_pci_sdmmc"
        "sdhci_pci"
        "sd_mod"
        "sr_mod"
        "usbhid"
        "usb_storage"
        "xhci_pci"
      ];
      kernelModules = [ ];
      postDeviceCommands = lib.mkAfter ''
        zfs rollback -r rpool/encrypted/local/root@blank && \
        echo "rollback complete"
      '';
    };
  };
}
