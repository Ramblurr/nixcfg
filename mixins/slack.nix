{pkgs, ...}: {
  home-manager.users.ramblurr = {
    pkgs,
    config,
    ...
  } @ hm: {
    home.packages = with pkgs; [
      slack
    ];

    home.persistence."/persist/home/ramblurr" = {
      directories = [
        ".config/slack"
      ];
    };
  };
}
