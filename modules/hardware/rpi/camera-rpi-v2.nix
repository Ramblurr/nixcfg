# Raspberry Pi V2 camera module (OV5647)
{
  lib,
  config,
  ...
}:

let
  cfg = config.modules.hardware.rpi.camera-rpi-v2;
  inherit (config.boot.kernelPackages) kernel;
in

{
  options = {
    modules.hardware.rpi.camera-rpi-v2 = {
      enable = lib.mkEnableOption ''
        support for the Raspberry Pi Camera v2
      '';

    };
  };
  config = lib.mkIf cfg.enable {
    hardware.deviceTree = {
      enable = true;
      overlays = [
        {
          name = "imx219.dtbo";
          dtsFile = lib.my.mkCompatibleDtsFile "${kernel}/dtbs/overlays/imx219.dtbo";
        }
      ];
    };
    modules.hardware.rpi.deviceTree.enable = true;
  };
}
