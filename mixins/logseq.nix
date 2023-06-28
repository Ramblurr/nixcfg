{pkgs, ...}: {
  home-manager.users.ramblurr = {
    pkgs,
    config,
    ...
  } @ hm: {
    home.packages = with pkgs; [
      logseq
    ];

    home.persistence."/persist/home/ramblurr" = {
      directories = [
        ".config/Logseq"
        ".logseq"
      ];
    };
  };
}
