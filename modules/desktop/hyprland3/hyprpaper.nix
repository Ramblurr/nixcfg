{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  cfg = config.modules.desktop.hyprland3;
  username = config.modules.users.primaryUser.username;
  hyprland = config.programs.hyprland.package;
in
{

  config = lib.mkIf cfg.enable {
    myhm =
      { ... }@hm:
      {
        services.hyprpaper = {
          enable = true;
          settings = {
            preload = [
              #"~/docs/img/walls/safe-landing.jpg"
              "~/docs/img/walls/hyprland/gruv-vertical.jpg"
              #"~/docs/img/walls/hyprland/abstract-4.png"
              #"~/docs/img/walls/safe-landing-vertical.jpg"
              "~/docs/img/walls/hyprland/gruvbox_astro_wide.jpg"
            ];
            wallpaper = [
              "HDMI-A-1,~/docs/img/walls/hyprland/gruv-vertical.jpg"
              "DP-2,contain:~/docs/img/walls/hyprland/gruvbox_astro_wide.jpg"
            ];
          };
        };
      };
  };
}
