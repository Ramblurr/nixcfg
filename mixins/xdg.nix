{
  pkgs,
  config,
  ...
}: {
  config = {
    xdg.portal = {
      enable = true;
      extraPortals = with pkgs; [
        # hyprland module enables its own portal
        # xdg-desktop-portal-hyprland
        libsForQt5.xdg-desktop-portal-kde
        #xdg-desktop-portal-gtk
      ];
    };
    home-manager.users.ramblurr = {pkgs, ...}: {
      home.packages = with pkgs; [
        xdg-utils
        xdg-user-dirs
      ];
      xdg = {
        enable = true;
        userDirs = {
          enable = true;
          desktop = "\$HOME/desktop";
          documents = "\$HOME/docs";
          download = "\$HOME/downloads";
          music = "\$HOME/tmp/music";
          pictures = "\$HOME/docs/img";
          publicShare = "\$HOME/docs/public";
          templates = "\$HOME/docs/templates";
          videos = "\$HOME/tmp/videos";
        };
      };
    };
  };
}
