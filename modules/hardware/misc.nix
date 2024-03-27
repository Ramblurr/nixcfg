{ config, lib, pkgs, ... }:
with lib;
with lib.my;
let cfg = config.modules.hardware;
in {
  options = {
    modules.hardware = {
      fwupd.enable = mkBoolOpt true;
      udisks2.enable = mkBoolOpt true;
      enableRedistributableFirmware = mkBoolOpt true;
      usbModeSwitch.enable = mkBoolOpt false;
    };
  };
  config = {
    ## MISC HARDWARE RELATED ################################################
    services.fwupd.enable = cfg.fwupd.enable;
    services.udisks2.enable = cfg.udisks2.enable;
    hardware.enableRedistributableFirmware = cfg.enableRedistributableFirmware;
    hardware.usb-modeswitch.enable = cfg.usbModeSwitch.enable;
    hardware.cpu.amd.updateMicrocode = pkgs.hostPlatform.system == "x86_64-linux";
    hardware.cpu.intel.updateMicrocode = pkgs.hostPlatform.system == "x86_64-linux";
  };
}
