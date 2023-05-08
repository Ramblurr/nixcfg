{ config, lib, pkgs, ... }:

{
    home-manager.users.ramblurr = {
      pkgs,
      config,
      ...
    } @ hm: {
      programs.atuin = {
        enable = true;
        settings = {
          style = "compact";
        };
      };
      home.persistence."/persist/home/ramblurr" = {
        directories = [
          ".config/atuin"
          ".local/share/atuin"
        ];
      };
    };
}
