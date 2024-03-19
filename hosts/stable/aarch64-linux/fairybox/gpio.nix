{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  #raspberry-pi.hardware.hifiberry-dac.enable = false;
  #raspberry-pi.hardware.spi0-1cs.enable = false;
  raspberry-pi.hardware.platform.type = "rpi4";
  raspberry-pi.hardware.apply-overlays-dtmerge.enable = lib.mkForce true;
  hardware = {
    firmware = [pkgs.wireless-regdb];
    i2c.enable = true;

    deviceTree = {
      enable = true;
      #filter = "bcm2711-rpi-4-b.dtb";
      overlays = [
        {
          # only enable 1 chip select pin, gpio 8, leaving the other pin free for gpio
          name = "spi0-0cs.dts";
          dtsText = builtins.readFile ./overlays/spi0-0cs-mainline.dts;
        }
        {
          name = "hifiberry-dac.dts";
          dtsText = builtins.readFile ./overlays/hifiberry-dac.dts;
        }
        {
          # Allows software made for the rpi to detect that it is in fact running on an rpi
          name = "rpi4-cpu-revision";
          dtsText = ''
            /dts-v1/;
            /plugin/;

            / {
              compatible = "raspberrypi,4-model-b";

              fragment@0 {
                target-path = "/";
                __overlay__ {
                  system {
                    linux,revision = <0x00d03114>;
                  };
                };
              };
            };
          '';
        }
      ];
    };
  };
}
