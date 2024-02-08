{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
  ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  #networking.useDHCP = lib.mkDefault true;
  networking.interfaces.enp0s9.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  # fileSystems."/" = {
  #   device = "rpool/encrypted/local/root";
  #   fsType = "zfs";
  #   neededForBoot = true;
  # };

  # fileSystems."/nix" = {
  #   device = "rpool/encrypted/local/nix";
  #   fsType = "zfs";
  #   neededForBoot = true;
  # };

  # fileSystems."/home" = {
  #   device = "rpool/encrypted/local/home";
  #   fsType = "zfs";
  #   neededForBoot = true;
  # };

  # fileSystems."/persist" = {
  #   device = "rpool/encrypted/safe/persist";
  #   fsType = "zfs";
  #   neededForBoot = true;
  # };

  # fileSystems."/persist/extra" = {
  #   device = "rpool/encrypted/safe/persist/extra";
  #   fsType = "zfs";
  #   neededForBoot = true;
  # };

  # fileSystems."/persist/extra/atuin" = {
  #   device = "rpool/encrypted/safe/persist/extra/atuin";
  #   fsType = "zfs";
  #   neededForBoot = true;
  # };

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
    kernelModules = ["kvm-amd"];
    extraModulePackages = [];

    initrd = {
      availableKernelModules = [
        "xhci_pci"
        "ohci_pci"
        "ehci_pci"
        "virtio_pci"
        "ahci"
        "usbhid"
        "sr_mod"
        "virtio_blk"
      ];
      kernelModules = ["usb_storage" "rbd"];

      postDeviceCommands = lib.mkAfter ''
        zfs rollback -r rpool/encrypted/local/root@blank && \
        echo "rollback complete"
      '';
    };
  };
}
