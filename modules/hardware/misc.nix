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
    # TODO(upgrade) hardware.usbWwan to hardware.usb-modeswitch once everything is on 23.11
    hardware.usbWwan.enable = cfg.usbModeSwitch.enable;
    # hardware.usb-modeswitch.enable = cfg.usbModeSwitch.enable;
    hardware.cpu.amd.updateMicrocode = pkgs.hostPlatform.system == "x86_64-linux";
    hardware.cpu.intel.updateMicrocode = pkgs.hostPlatform.system == "x86_64-linux";
  };
}
