{
  config,
  lib,
  pkgs,
  ...
}: {
  home-manager.users.ramblurr = {
    pkgs,
    lib,
    ...
  }: {
    home.persistence."/persist/home/ramblurr" = {
      directories = [
        ".config/kdeconnect"
      ];
    };

    services.kdeconnect = {
      enable = true;
      indicator = true;
    };
  };
}
