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
      timeout = 1;
      efi.canTouchEfiVariables = true;
      systemd-boot = {
        enable = true;
        configurationLimit = 20;
      };
    };
    kernelModules = [ "kvm-intel" ];
    initrd = {
      availableKernelModules = [
        "xhci_pci"
        "ahci"
        "nvme"
        "usbhid"
        "usb_storage"
        "sd_mod"
        "sr_mod"
        "sdhci_pci"
      ];
      systemd.services.rollback.script = lib.mkForce ''
        zfs rollback -r rpool/encrypted/local/root@blank && \
        echo "rollback complete"
      '';
    };
  };

  services.udev.extraRules = ''
    # Authorize Sonnet 10GbE Thunderbolt devices
    ACTION=="add", SUBSYSTEM=="thunderbolt", ATTR{vendor}=="0x8", ATTR{device}=="0x36", ATTR{authorized}="1"
  '';
}
