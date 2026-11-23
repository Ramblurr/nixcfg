{
  lib,
  pkgs,
  ...
}:

{
  imports = [ ../../config/rpi4-hardware.nix ];
  modules.hardware.rpi.type = "rpi4";
  # The nixos-raspberrypi vendor kernel is in their binary cache; the nixpkgs
  # linux_rpi4 forced by rpi4-hardware.nix is not, and takes hours on ARM.
  boot.kernelPackages = lib.mkOverride 40 pkgs.rpi.linuxPackages_rpi4;
  boot.loader = {
    generic-extlinux-compatible.enable = true;
    grub.enable = false;
  };
}
