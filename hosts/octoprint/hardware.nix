{
  ...
}:

{
  imports = [ ../../config/rpi4-hardware.nix ];
  modules.hardware.rpi.type = "rpi4";
  boot.loader = {
    generic-extlinux-compatible.enable = true;
    grub.enable = false;
  };
}
