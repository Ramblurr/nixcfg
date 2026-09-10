{ lib, ... }:
{
  # Facts supplied from nixos-generate-config on the ThinkPad X13 Yoga Gen 1.
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  boot = {
    initrd.availableKernelModules = [
      "xhci_pci"
      "nvme"
      "usb_storage"
      "sd_mod"
      "sdhci_pci"
    ];
    # Disko supplies filesystems and encrypted swap in disk-config.nix.
    loader = {
      systemd-boot = {
        enable = true;
        configurationLimit = 10;
      };
      efi.canTouchEfiVariables = true;
    };
  };
  hardware = {
    cpu.intel.updateMicrocode = true;
    enableRedistributableFirmware = true;
    graphics.enable = true;
  };
  services.libinput.enable = true;
}
