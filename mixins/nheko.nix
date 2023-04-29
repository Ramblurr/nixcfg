{
  config,
  lib,
  pkgs,
  ...
}: {
  home-manager.users.ramblurr = {pkgs, ...} @ hm: {
    home.packages = with pkgs; [
      nheko
    ];
    home.persistence."/persist/home/ramblurr" = {
      directories = [
        ".config/nheko"
        ".cache/nheko"
        ".local/share/nheko"
      ];
    };
  };
}
