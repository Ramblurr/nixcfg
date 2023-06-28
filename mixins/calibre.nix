{pkgs, ...}: {
  home-manager.users.ramblurr = {
    pkgs,
    config,
    ...
  } @ hm: {
    home.packages = with pkgs; [
      calibre
    ];

    home.persistence."/persist/home/ramblurr" = {
      directories = [
        ".config/calibre"
      ];
    };
  };
}
