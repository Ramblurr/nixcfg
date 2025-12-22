{
  lib,
  inputs,
  ...
}:
{
  imports = [ inputs.nixos-hardware.nixosModules.framework-13-7040-amd ];
  # The firmware on the fingerprint sensor needs a downgrade to make it work on Linux. The process is documented here.
  # However on recent NixOS versions also fwupd can no longer update the firmware. Using the following snippet allows
  # to temporarly downgrade fwupd to an old-enough version:
  services.fwupd = {
    enable = true;
    # we need fwupd 1.9.7 to downgrade the fingerprint sensor firmware
    #  package =
    #    (import (builtins.fetchTarball {
    #      url = "https://github.com/NixOS/nixpkgs/archive/bb2009ca185d97813e75736c2b8d1d8bb81bde05.tar.gz";
    #      sha256 = "sha256:003qcrsq5g5lggfrpq31gcvj82lb065xvr7bpfa8ddsw8x4dnysk";
    #    }) { inherit (pkgs) system; }).fwupd;
  };

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
    extraModulePackages = [ ];
    kernelModules = [ "kvm-amd" ];
    initrd = {
      availableKernelModules = [
        "nvme"
        "xhci_pci"
        "thunderbolt"
        "usb_storage"
        "sd_mod"
      ];
      kernelModules = [ ];
      postDeviceCommands = lib.mkAfter ''
        zfs rollback -r rpool/encrypted/local/root@blank && \
        echo "rollback complete"
      '';
    };
  };

  hardware = {
    #bluetooth = { enable = true; powerOnBoot = false; };
    enableRedistributableFirmware = true; # enables microcode updates
  };
}
