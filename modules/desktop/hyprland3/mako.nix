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
          settings = {
            font = "monospace 18";
            anchor = "top-center";
            markup = true;
            padding = "5";
            background-color = "#282828";
            text-color = "#ebdbb2";
            progress-color = "#ebdbb2";
            border-color = "#928374";
            border-size = 3;
            border-radius = 6;
            width = 600;
          };
        };
      };
  };
}
