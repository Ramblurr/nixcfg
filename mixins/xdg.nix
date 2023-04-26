{
  pkgs,
  config,
  ...
}: {
  config = {
    # hyprland module enables xdg.portal
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
