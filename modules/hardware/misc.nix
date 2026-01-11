{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.hardware;
in
{
  options = {
    modules.hardware = {
      fwupd.enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
      };
      udisks2.enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
      };
      enableRedistributableFirmware = lib.mkOption {
        type = lib.types.bool;
        default = true;
      };
      usbModeSwitch.enable = lib.mkEnableOption "";
    };
  };
  config = {
    ## MISC HARDWARE RELATED ################################################
    services.fwupd.enable = cfg.fwupd.enable;
    services.udisks2.enable = cfg.udisks2.enable;
    hardware.enableRedistributableFirmware = cfg.enableRedistributableFirmware;
    hardware.usb-modeswitch.enable = cfg.usbModeSwitch.enable;
    hardware.cpu.amd.updateMicrocode = pkgs.stdenv.hostPlatform.system == "x86_64-linux";
    hardware.cpu.intel.updateMicrocode = pkgs.stdenv.hostPlatform.system == "x86_64-linux";
  };
}
