{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.hardware.om-footswitch;
in
{
  options = {
    modules.hardware.om-footswitch = {
      enable = lib.mkEnableOption "";
      keys = {
        left = lib.mkOption {
          type = lib.types.str;
          default = "previoussong";
        };
        right = lib.mkOption {
          type = lib.types.str;
          default = "f14";
        };
        middle = lib.mkOption {
          type = lib.types.str;
          default = "f15";
        };
      };
    };
  };

  config = mkIf cfg.enable {
    services.udev.extraRules = ''
      # OM Digital Solutions HID FootSwitch RS Series (33a2:0218)
      # it presents as a mouse, make it a keyboard instead
      SUBSYSTEM=="input", ATTRS{idVendor}=="33a2", ATTRS{idProduct}=="0218", ENV{ID_INPUT_KEYBOARD}="1", ENV{ID_INPUT_MOUSE}="0"
    '';
    services.udev.extraHwdb = ''
      # OM Digital Solutions HID FootSwitch RS Series (33a2:0218)
      # left, right, middle
      evdev:input:b0003v33A2p0218*
       KEYBOARD_KEY_00090011=${cfg.keys.left}
       KEYBOARD_KEY_00090012=${cfg.keys.right}
       KEYBOARD_KEY_00090013=${cfg.keys.middle}
    '';
  };
}
