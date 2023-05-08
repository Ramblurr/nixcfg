{ config, lib, pkgs, ... }:

{
    home-manager.users.ramblurr = {
      pkgs,
      config,
      ...
    } @ hm: {
      programs.atuin = {
        enable = true;
      };
      home.persistence."/persist/home/ramblurr" = {
        directories = [
          ".config/atuin"
          ".local/share/atuin"
        ];
      };
    };
}
