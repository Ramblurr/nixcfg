{
  config,
  lib,
  ...
}:

let
  cfg = config.modules.desktop.hyprpaper;
in
{

  options.modules.desktop.hyprpaper = {
    enable = lib.mkEnableOption "Enable hyprpaper";
  };
  config = lib.mkIf cfg.enable {
    myhm = _: {
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
