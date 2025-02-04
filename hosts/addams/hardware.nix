{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:
{

  imports = [
    (modulesPath + "/profiles/all-hardware.nix")
    ./disk-config.nix
  ];

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  services.getty.autologinUser = "ramblurr";
  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    zfs.devNodes = lib.mkForce "/dev/disk/by-partuuid";
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];

    initrd = {
      availableKernelModules = [
        "xhci_pci"
        "thunderbolt"
        "ahci"
        "usbhid"
        "usb_storage"
        "sd_mod"
        "sr_mod"
      ];
      kernelModules = [ ];

      postDeviceCommands = lib.mkAfter ''
        zfs rollback -r rpool/encrypted/local/root@blank && \
        echo "rollback complete"
      '';
    };
  };
}
