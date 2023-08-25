{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  prefs = import ./_preferences.nix {inherit inputs config lib pkgs;};
in {
  home-manager.users.ramblurr = {pkgs, ...} @ hm: {
    gtk = {
      enable = true;
      font = prefs.gtk.font;
      theme = prefs.gtk.theme;
      iconTheme = prefs.gtk.iconTheme;
      cursorTheme = prefs.cursor;
      gtk2.configLocation = "${hm.config.xdg.configHome}/gtk-2.0/gtkrc";
      gtk3.extraConfig = {
        gtk-application-prefer-dark-theme = 1;
        gtk-cursor-theme-size = prefs.cursorSize;
        gtk-xft-hinting = 1;
        gtk-xft-hintstyle = "slight";
        gtk-xft-antialias = 1; # => font-antialiasing="grayscale"
        gtk-xft-rgba = "rgb"; # => font-rgb-order="rgb"
      };
    };
  };
}
