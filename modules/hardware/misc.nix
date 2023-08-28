{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.hardware;
in {
  options = {
    modules.hardware = {
      fwupd.enable = mkBoolOpt true;
      udisk2.enable = mkBoolOpt true;
      enableRedistributableFirmware = mkBoolOpt true;
      usbModeSwitch.enable = mkBoolOpt false;
    };
  };
  config = {
    ## MISC HARDWARE RELATED ################################################
    services.fwupd.enable = cfg.fwupd.enable;
    services.udisks2.enable = cfg.udisk2.enable;
    hardware.enableRedistributableFirmware = cfg.enableRedistributableFirmware;
    hardware.usb-modeswitch.enable = cfg.usbModeSwitch.enable; # dual role usb/cdrom stick thing
    hardware.cpu.amd.updateMicrocode = pkgs.hostPlatform.system == "x86_64-linux";
    hardware.cpu.intel.updateMicrocode = pkgs.hostPlatform.system == "x86_64-linux";
  };
}
