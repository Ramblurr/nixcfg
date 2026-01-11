{
  ...
}:

{
  imports = [ ../../config/rpi4-hardware.nix ];
  #boot.loader.grub.device = "/dev/sda";
  #boot.loader.timeout = 0;
  boot.loader = {
    generic-extlinux-compatible.enable = true;
    grub.enable = false;
  };
  # Disable on board sound
  boot.kernelParams = [
    "snd_bcm2835.enable_headphones=0"
    "snd_bcm2835.enable_hdmi=0"
  ];
  modules.hardware.rpi = {
    type = "rpi4";
    hifiberry-dac-plus.enable = true;
  };
}
