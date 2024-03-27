{ options, config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.wayland;
  username = config.modules.users.primaryUser.username;
in {
  options.modules.desktop.wayland = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    myhm = {
      home.sessionVariables = {
        XDG_SESSION_TYPE = "wayland";
        NIXOS_OZONE_WL = "1";
        MOZ_ENABLE_WAYLAND = "1";
        _JAVA_AWT_WM_NONREPARENTING = "1";
      };
      home.packages = with pkgs; [
        qt5.qtwayland
        qt6.qtwayland

        wl-clipboard # wl-{copy,paste}
        wtype # virtual keystroke insertion

        # imv # image viewer
        oculante # image viewer (rust)
        grim # area selection
        slurp # screen capture
        wf-recorder # screen record
        wev # event viewer
      ];
    };
  };
}
