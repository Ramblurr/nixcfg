{
  pkgs,
  lib,
  config,
  inputs,
  ...
}: {
  imports = [
    ./gui.nix
    ../mixins/obs.nix
  ];
  config = {
    home-manager.users.ramblurr = {pkgs, ...}: {
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
