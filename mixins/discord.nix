{pkgs, ...}: {
  home-manager.users.ramblurr = {
    pkgs,
    config,
    ...
  } @ hm: {
    home.packages = with pkgs; [
      discord
      betterdiscordctl
    ];

    home.persistence."/persist/home/ramblurr" = {
      directories = [
        ".config/BetterDiscord"
        ".config/discord"
      ];
    };
  };
}
