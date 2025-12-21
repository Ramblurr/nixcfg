{
  lib,
  pkgs,
  modulesPath,
  config,
  ...
}:
{
  imports = [
    ../../config/rpi4-sd-image.nix
  ];

  boot.loader.grub.device = "/dev/sda";
  boot.loader.timeout = 0;
  networking = {
    hostName = "nixos-rpi";
    useNetworkd = true;
  };
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

  # Replace networkd with NetworkManager at your discretion
  systemd = {
    network = {
      enable = true;
      networks."10-lan" = {
        matchConfig.Name = "end0";
        networkConfig.DHCP = "yes";
        linkConfig.RequiredForOnline = "routable";
      };
    };
  };

  users.users.pi = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = config.repo.secrets.global.pubKeys;
  };

  # Our user doesn't have a password, so we let them
  # do sudo without one
  security.sudo.wheelNeedsPassword = false;
  services = {
    openssh.enable = true;
  };
  time.timeZone = "Europe/Berlin";
  environment.systemPackages = with pkgs; [
    libraspberrypi
    raspberrypi-eeprom
  ];
  hardware.enableRedistributableFirmware = true;
  system.stateVersion = "25.11";
}
