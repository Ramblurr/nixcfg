{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.hardware.rpi;
  dt_ao_overlay = _final: prev: {
    deviceTree = prev.deviceTree // {
      applyOverlays = _final.callPackage ./apply-overlays-dtmerge.nix { };
    };
  };
in
{
  options.modules.hardware.rpi = {
    type = lib.mkOption {
      type = lib.types.enum [
        "rpi3"
        "rpi4"
        "rpizero2"
      ];
      default = null;
      description = ''
        The Raspberry Pi Platform the build is targeting.
      '';
    };
    deviceTreeFilter = lib.mkOption {
      type = lib.types.str;
      default = null;
      description = ''
        A string to filter the device tree files by.
      '';
    };
    deviceTree.enable = lib.mkEnableOption "";
  };
  config = {
    modules.hardware.rpi.deviceTreeFilter =
      if cfg.type == "rpi4" then
        "bcm2711-rpi-4*.dtb"
      else if cfg.type == "rpi3" then
        "bcm*-rpi-3-*.dtb"
      else if cfg.type == "rpizero2" then
        "bcm2837-rpi-zero-2-w.dtb"
      else
        "";

    nixpkgs = lib.mkIf cfg.deviceTree.enable {
      overlays = [ dt_ao_overlay ];
    };
    hardware = lib.mkIf cfg.deviceTree.enable {
      firmware = [ pkgs.wireless-regdb ];
      i2c.enable = true;
      deviceTree = {
        enable = true;
        filter = cfg.deviceTreeFilter;
      };
    };
  };
}
