{
  config,
  lib,
  pkgs,
  ...
}: {
  # Using flatpak 1password for now because native wayland 1password has a broken clipboard
  #users.users.ramblurr.packages = with pkgs; [
  #  _1password-gui
  #];

  home-manager.users.ramblurr = {pkgs, ...} @ hm: {
    home.persistence."/persist/home/ramblurr" = {
      directories = [
        ".config/1Password"
      ];
    };
  };
}
