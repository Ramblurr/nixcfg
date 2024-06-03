{
  config,
  pkgs,
  lib,
  inputs,
  unstable,
  ...
}:
{
  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.kernelPatches = [
    {
      # required on mainlin kernels
      name = "allow-devmem";
      patch = null;
      extraConfig = ''
        STRICT_DEVMEM n
      '';
    }
  ];

  boot.kernelParams = [
    "snd_bcm2835.enable_hdmi=0"
    "iomem=relaxed"
  ];
  boot.blacklistedKernelModules = [ "snd_bcm2835" ];
  boot.kernelModules = [
    "pwm_bcm2835"
    "w1-gpio"
  ];
}
