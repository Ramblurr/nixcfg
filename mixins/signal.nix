{pkgs, ...}: {
  home-manager.users.ramblurr = {
    pkgs,
    config,
    ...
  } @ hm: {
    home.packages = with pkgs; [
      signal-desktop
    ];

    home.persistence."/persist/home/ramblurr" = {
      directories = [
         ".config/Signal"
      ];
    };
  };
}
