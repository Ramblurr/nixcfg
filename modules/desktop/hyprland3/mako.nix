{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.desktop.hyprland3;
  username = config.modules.users.primaryUser.username;
in
{
  config = lib.mkIf cfg.enable {
    myhm =
      { pkgs, ... }@hm:
      {
        services.mako = {
          enable = true;
          anchor = "top-center";
          markup = true;
          padding = "5";
          #backgroundColor = "#689d6a";
          #progressColor = "#ebdbb2";
          #textColor = "#1d2021";

          #borderColor = "#ebdbb2";
          #borderSize = 5;
          #borderRadius = 2;

          #
          backgroundColor = "#282828";
          textColor = "#ebdbb2";
          progressColor = "#ebdbb2";
          borderColor = "#928374";
          borderSize = 3;
          borderRadius = 6;
        };
      };
  };
}
