{
  config,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ../modules/dynamic-wallpaper.nix
  ];
  config = {
    environment.systemPackages = with pkgs; [
      (python311.withPackages(ps: with ps; [ requests]))
    ];
    services.dynamic-wallpaper = {
      enable = true;
      transitionDuration = 10;
    };
    home-manager.users.ramblurr = {pkgs, ...} @ hm: {
      home.file = {
        # Add hyprland config
        ".config/hypr/hyprland.conf" = {
          source = ../configs/hyprland.conf;
          recursive = true;
        };
        # Add hyprland portal wrapper
        ".config/hypr/xdg-portal-hyprland" = {
          source = ../configs/xdg-portal-hyprland;
          recursive = true;
        };

        # Add wallpaper files
        ".config/hypr/wallpaper" = {
          source = ../configs/wallpaper;
          recursive = true;
          force = true;
        };

        # Add waybar config files
        ".config/waybar" = {
          source = ../configs/waybar;
          recursive = true;
        };

        # Add rofi config files
        ".config/rofi/config.rasi" = {
          source = ../configs/rofi/config.rasi;
          recursive = true;
        };

        ".config/rofi/theme.rasi" = {
          source = ../configs/rofi/theme.rasi;
          recursive = true;
        };

        # Add dunst config file
        ".config/dunst/dunstrc" = {
          source = ../configs/dunstrc;
          recursive = true;
        };

        # Add swaylock config files
        ".config/swaylock" = {
          source = ../configs/swaylock;
          recursive = true;
        };

        # Add wlogout config files
        ".config/wlogout/layout" = {
          source = ../configs/wlogout/layout;
          recursive = true;
        };

        ".config/wlogout/style.css" = {
          source = ../configs/wlogout/style.css;
          recursive = true;
        };

        # Avoid file not found errors for bash
        ".bashrc" = {
          text = '''';
          recursive = true;
        };
      };
    };
  };
}
