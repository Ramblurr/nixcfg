{ config, lib, pkgs, ... }:

{
  services.mullvad-vpn = {
    enable = true;
    package = pkgs.mullvad-vpn;
    enableExcludeWrapper = true;
  };

  home-manager.users.ramblurr = {pkgs, ...} @ hm: {
    home.persistence."/persist/home/ramblurr" = {
      directories = [
          {
            method = "symlink";
            directory = ".config/Mullvad VPN";
          }
      ];
    };
  };
}
