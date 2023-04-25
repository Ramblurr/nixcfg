{
  config,
  lib,
  pkgs,
  ...
}: {
  users.users.ramblurr.packages = with pkgs; [
    _1password-gui
  ];

  home-manager.users.ramblurr = {pkgs, ...} @ hm: {
    home.persistence."/persist/home/ramblurr" = {
      directories = [
        ".config/1Password"
      ];
    };
  };
}
