{ config, lib, pkgs, ... }:

{
    home-manager.users.ramblurr = {
      pkgs,
      config,
      ...
    } @ hm: {
      programs.zoxide = {
        enable = true;
      };
      home.persistence."/persist/home/ramblurr" = {
        directories = [
          ".local/share/zoxide"
        ];
      };
    };
}
