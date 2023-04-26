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
      (python311.withPackages (ps: with ps; [requests]))
    ];
    services.dynamic-wallpaper = {
      enable = true;
      transitionDuration = 10;
    };

    home-manager.users.ramblurr = {pkgs, ...} @ hm: {
      services.swayidle = {
        enable = true;
        events = [
          {
            event = "before-sleep";
            command = "${pkgs.swaylock}/bin/swaylock -f";
          }
          {
            event = "lock";
            command = "lock";
          }
        ];
        timeouts = [
          {
            timeout = 300;
            command = "${pkgs.swaylock}/bin/swaylock -f";
          }
        ];
        systemdTarget = "xdg-desktop-portal-hyprland.service";
      };
      programs.swaylock = {
        enable = true;
        settings = {
          #image = "$HOME/.config/wall";
          color = "000000f0";
          font-size = "24";
          indicator-idle-visible = false;
          indicator-radius = 100;
          indicator-thickness = 20;
          inside-color = "00000000";
          inside-clear-color = "00000000";
          inside-ver-color = "00000000";
          inside-wrong-color = "00000000";
          key-hl-color = "79b360";
          line-color = "000000f0";
          line-clear-color = "000000f0";
          line-ver-color = "000000f0";
          line-wrong-color = "000000f0";
          ring-color = "ffffff50";
          ring-clear-color = "bbbbbb50";
          ring-ver-color = "bbbbbb50";
          ring-wrong-color = "b3606050";
          text-color = "ffffff";
          text-ver-color = "ffffff";
          text-wrong-color = "ffffff";
          show-failed-attempts = true;
        };
      };
      home.file = {
        # Add hyprland config
        ".config/hypr/hyprland.conf" = {
          source = ../configs/hyprland.conf;
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
