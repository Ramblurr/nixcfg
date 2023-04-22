{
  pkgs,
  config,
  ...
}: {
  config = {
    # see profiles/desktop-<foo>.nix for xdg-portal stuff

    home-manager.users.cole = {pkgs, ...}: {
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
