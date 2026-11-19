{ modulesPath, ... }:
{
  imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];
  boot.initrd.availableKernelModules = [
    "ata_piix"
    "uhci_hcd"
    "xen_blkfront"
    "vmw_pvscsi"
  ];
  boot.initrd.kernelModules = [ "nvme" ];

  zramSwap.enable = true;
  # zram has no locality, so skip swap readahead.
  boot.kernel.sysctl."vm.page-cluster" = 0;
}
