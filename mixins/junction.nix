{
  config,
  lib,
  pkgs,
  ...
}: {
  home-manager.users.ramblurr = {pkgs, ...}: {
    home.packages = [pkgs.junction];
    home.persistence."/persist/home/ramblurr" = {
      directories = [
        {
          method = "symlink";
          directory = ".config/mimeapps.list";
        }
      ];
    };
  };
}
