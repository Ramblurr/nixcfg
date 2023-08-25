{pkgs, ...}: {
  home-manager.users.ramblurr = {pkgs, ...}: {
    programs.mpv = {
      enable = true;
    };
  };
}
