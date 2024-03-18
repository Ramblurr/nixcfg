{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  build-overlay = overlay:
    pkgs.runCommandCC overlay {nativeBuildInputs = [pkgs.dtc];} ''
      mkdir $out
      cd $out
      builddtb() {
        $CC -x assembler-with-cpp -E $1 -o temp
        # egrep -v '^#' < temp > temp2
        dtc temp -o $2
        rm temp
        # temp2
      }
      builddtb ${./overlays}/${overlay}.dts ${overlay}.dtb
    '';
  #ov5647 = build-overlay "ov5647";
  #disable-bt = build-overlay "disable-bt";
  #uart0 = build-overlay "uart0";
  #rpi-ft5406 = build-overlay "rpi-ft5406";
  #spi0 = build-overlay "spi0";
  #spi4 = build-overlay "spi4";
  #spi6 = build-overlay "spi6";
in {
  #raspberry-pi.hardware.hifiberry-dac.enable = false;
  #raspberry-pi.hardware.spi0-1cs.enable = false;
  raspberry-pi.hardware.platform.type = "rpi4";

  raspberry-pi.hardware.apply-overlays-dtmerge.enable = lib.mkForce false;
  hardware = {
    firmware = [pkgs.wireless-regdb];
    i2c.enable = true;

    deviceTree = {
      enable = true;
      filter = "bcm2711-rpi-4-b.dtb";
      overlays = [
        {
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
