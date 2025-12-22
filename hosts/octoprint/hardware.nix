{
  lib,
  pkgs,
  ...
}:

{
  #imports = [ inputs.nixos-hardware.nixosModules.raspberry-pi-4 ];
  boot.loader.grub.device = "/dev/sda";
  boot.loader.timeout = 0;
  # Disable unnecessary filesystems.
  # RPi only needs ext4 for root and vfat for boot firmware.
  boot.supportedFilesystems = lib.mkForce {
    ext4 = true;
    vfat = true;
    zfs = false;
  };
  # Disable unnecessary filesystems.
  # RPi only needs ext4 for root and vfat for boot firmware.
  boot.initrd.supportedFilesystems = lib.mkForce {
    ext4 = true;
    vfat = true;
    zfs = false;
  };
  # Root filesystem on SD card
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = [ "noatime" ];
    };
  };
  swapDevices = [ ];
  # Disable unneeded features
  services = {
    avahi.enable = false;
    nfs.server.enable = false;
    samba.enable = false;
    journald.extraConfig = ''
      Storage = volatile
      RuntimeMaxFileSize = 50M
    '';
  };
  boot.kernelPackages = lib.mkForce pkgs.linuxKernel.packages.linux_rpi4;
  boot.kernelParams = [ "snd_bcm2835.enable_hdmi=1" ];
  boot.kernelModules = [ "hidp" ];
  # Disable Bluetooth to free ttyAMA0 for serial devices like GPS, 3D printers.
  # On RPi4 this restores PL011 UART to GPIO 14/15 and creates /dev/ttyAMA0.
  hardware.bluetooth.enable = false;
  boot.blacklistedKernelModules = [ "hci_uart" ];
  environment.systemPackages = with pkgs; [
    usbutils
    libraspberrypi
    raspberrypi-eeprom
    iw # iw is for WiFi diagnostics and TX power adjustment
  ];
  nixpkgs.overlays = [
    #(final: super: {
    #  zfs = super.zfs.overrideAttrs (_: {
    #    meta.platforms = [ ];
    #  });
    #})
    # Workaround: https://github.com/NixOS/nixpkgs/issues/154163
    # modprobe: FATAL: Module sun4i-drm not found in directory
    (_final: super: {
      makeModulesClosure = x: super.makeModulesClosure (x // { allowMissing = true; });
    })
  ];
  hardware.enableAllHardware = lib.mkForce false;
  # Disable documentation since RPi is deployed remotely.
  documentation = {
    enable = false;
    doc.enable = false;
    info.enable = false;
    man.enable = false;
    nixos.enable = false;
  };
}
