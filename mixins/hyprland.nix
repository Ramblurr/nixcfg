{
  config,
  pkgs,
  inputs,
  ...
}: let
  hyprland_waybar = pkgs.waybar.overrideAttrs (oldAttrs: {
    mesonFlags = oldAttrs.mesonFlags ++ ["-Dexperimental=true"];
    postPatch = ''
      sed -i 's/zext_workspace_handle_v1_activate(workspace_handle_);/const std::string command = "hyprctl dispatch workspace " + name_;\n\tsystem(command.c_str());/g' src/modules/wlr/workspace_manager.cpp
    '';
  });
in {
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

    # see https://github.com/NixOS/nixpkgs/issues/158025
    security.pam.services.swaylock = {};
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
      services.dunst = {
        enable = true;
        configFile = ../configs/dunstrc;
      };
      programs.waybar = {
        enable = true;
        package = hyprland_waybar;
        systemd.enable = true;
      };
      programs.wlogout = {
        enable = true;
        layout = [
          {
            "label" = "logout";
            "action" = "loginctl terminate-user $USER";
            "text" = "Logout";
            "keybind" = "l";
          }

          {
            "label" = "reboot";
            "action" = "systemctl reboot";
            "text" = "Reboot";
            "keybind" = "r";
          }

          {
            "label" = "shutdown";
            "action" = "systemctl poweroff";
            "text" = "Power Off";
            "keybind" = "s";
          }
        ];
        style = ''

          * {
            background-image: none;
          }
          window {
            background-color: rgba(12, 12, 12, 1);
          }
          button {
            color: #FFFFFF;
            background-color: #1E1E1E;
            border-radius: 20px;
            background-repeat: no-repeat;
            background-position: center;
            background-size: 50%;
            margin: 10px;
          }

          button:hover {
            background-color: #3b393d;
            outline-style: none;
          }
        '';
      };
      programs.swaylock = {
        enable = true;
        settings = {
          show-failed-attempts = true;
          clock = true;
          indicator = true;
          effect-blur = "5x5";
          color = "1f1d2e80";
          font = "Iosevka Comfy Fixed";
          indicator-radius = 200;
          indicator-thickness = 20;
          line-color = "1f1d2e";
          ring-color = "191724";
          inside-color = "1f1d2e";
          key-hl-color = "eb6f92";
          separator-color = "00000000";
          text-color = "e0def4";
          text-caps-lock-color = "";
          line-ver-color = "eb6f92";
          ring-ver-color = "eb6f92";
          inside-ver-color = "1f1d2e";
          text-ver-color = "e0def4";
          ring-wrong-color = "31748f";
          text-wrong-color = "31748f";
          inside-wrong-color = "1f1d2e";
          inside-clear-color = "1f1d2e";
          text-clear-color = "e0def4";
          ring-clear-color = "9ccfd8";
          line-clear-color = "1f1d2e";
          line-wrong-color = "1f1d2e";
          bs-hl-color = "31748f";
          grace = 2;
          grace-no-mouse = true;
          grace-no-touch = true;
          datestr = "%d.%m";
          fade-in = "0.1";
          ignore-empty-password = true;
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
      };
    };
  };
}
