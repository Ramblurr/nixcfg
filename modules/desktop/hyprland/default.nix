{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.hyprland;
  username = config.modules.users.primaryUser.username;
  hy3_layouter = pkgs.writeScriptBin "hy3-layouter.sh" (builtins.readFile ./configs/scripts/hy3-layouter.sh);
  hyprland_waybar = pkgs.waybar.overrideAttrs (oldAttrs: {
    mesonFlags = oldAttrs.mesonFlags ++ ["-Dexperimental=true"];
    postPatch = ''
      sed -i 's/zext_workspace_handle_v1_activate(workspace_handle_);/const std::string command = "hyprctl dispatch workspace " + name_;\n\tsystem(command.c_str());/g' src/modules/wlr/workspace_manager.cpp
    '';
  });
  startupScript = pkgs.writeScript "hyprland-startup.sh" (builtins.readFile ./hyprland-startup.sh);
in {
  options.modules.desktop.hyprland = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    modules.desktop.wayland.enable = true;
    services = {
      dbus.enable = true;
      gvfs.enable = true; # Needed for nautilus
    };
    services.dynamic-wallpaper = {
      enable = true;
      transitionDuration = 10;
    };
    programs = {
      kdeconnect.enable = true; # Connect phone to PC
      hyprland.enable = true;
      hyprland.package = inputs.hyprland.packages.${pkgs.system}.hyprland;
      # TODO(upgrade) `hyprland.nvidiaPatches` to `hyprland.enableNvidiaPatches`
      hyprland.nvidiaPatches = true;
      #hyprland.enableNvidiaPatches = true;
    };

    security.polkit.enable = true;
    security.pam.services.kwallet = {
      name = "kwallet";
      enableKwallet = true;
    };
    # see https://github.com/NixOS/nixpkgs/issues/158025
    security.pam.services.swaylock = {};
    systemd.user.services.polkit-gnome-authentication-agent-1 = {
      description = "polkit-gnome-authentication-agent-1";
      wantedBy = ["hyprland-session.target"];
      wants = ["hyprland-session.target"];
      after = ["hyprland-session.target"];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
    };
    environment = {
      systemPackages = with pkgs; [
        (python311.withPackages (ps: with ps; [requests]))
        wlr-randr
        waypipe
        baobab # Disk usage analyser
        blueberry # Bluetooth manager
        clipman # Clipboard manager for wayland
        gnome.file-roller # Archive file manager
        gnome.gnome-calculator # Calculator
        gnome.gnome-disk-utility # Disks manager
        gnome.gnome-themes-extra # Adwaita GTK theme
        gnome.nautilus # File manager
        grim # Screenshot tool
        jq # JSON parser
        pavucontrol # Sound manager
        polkit_gnome # Polkit manager
        rofi-wayland # App launcher
        slurp # Monitor selector
        swappy # Edit screenshots
        wdisplays # Displays manager
        wl-clipboard # Clipboard daemon
        hyprpicker # color picker
        gtk-layer-shell # shell components on wayland
        #inputs.hyprNStack.packages.${pkgs.system}.hyprNStack
        inputs.hy3.packages.x86_64-linux.hy3
      ];
    };

    home-manager.users."${username}" = {pkgs, ...} @ hm: {
      home.packages = [
        hy3_layouter
      ];

      home.file.".local/bin/hyprland-startup.sh" = {
        enable = true;
        executable = true;
        source = startupScript;
      };
      systemd.user.targets.hyprland-session = {
        Unit = {
          Description = "hyprland compositor session";
          Documentation = ["man:systemd.special(7)"];
          BindsTo = ["graphical-session.target"];
          Wants = ["graphical-session-pre.target"];
          After = ["graphical-session-pre.target"];
        };
      };
      services.swayidle = {
        enable = true;
        # events = [
        #   {
        #     event = "before-sleep";
        #     command = "${pkgs.pkgs.swaylock-effects}/bin/swaylock -f --config ${hm.config.home.homeDirectory}/.config/swaylock/config";
        #   }
        #   {
        #     event = "lock";
        #     command = "lock";
        #   }
        # ];
        timeouts = [
          {
            timeout = 549;
            command = "${pkgs.libnotify}/bin/notify-send -t 15000 -u critical -i /etc/profiles/per-user/${username}/share/icons/breeze/preferences/22/preferences-desktop-screensaver.svg  \"Idle timeout\" \"Screen is locking in 1 minute\"";
          }
          #{
          #  timeout = 500;
          #  command = "${pkgs.swaylock-effects}/bin/swaylock -f --config ${hm.config.home.homeDirectory}/.config/swaylock/config";
          #}
          {
            timeout = 550;
            command = "${pkgs.hyprland}/bin/hyprctl dispatch dpms off";
            resumeCommand = "${pkgs.hyprland}/bin/hyprctl dispatch dpms on";
          }
        ];
        systemdTarget = "xdg-desktop-portal-hyprland.service";
      };
      services.dunst = {
        enable = true;
        configFile = ./configs/dunstrc;
      };
      programs.waybar = {
        enable = true;
        package = hyprland_waybar;
        systemd.enable = true;
        systemd.target = "hyprland-session.target";
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
          #logout {
              background-image: image(url("${pkgs.wlogout}/share/wlogout/icons/logout.png"), url("/usr/local/share/wlogout/icons/logout.png"));
          }

          #shutdown {
              background-image: image(url("${pkgs.wlogout}/share/wlogout/icons/shutdown.png"), url("/usr/local/share/wlogout/icons/shutdown.png"));
          }

          #reboot {
              background-image: image(url("${pkgs.wlogout}/share/wlogout/icons/reboot.png"), url("/usr/local/share/wlogout/icons/reboot.png"));
          }
        '';
      };
      programs.swaylock = {
        enable = true;
        package = pkgs.swaylock-effects;
        settings = {
          show-failed-attempts = true;
          clock = true;
          datestr = "%d.%m";
          indicator = true;
          effect-blur = "5x5";
          color = "1d2021";
          font = "Iosevka Comfy Fixed";
          indicator-radius = 200;
          indicator-thickness = 20;
          line-color = "d79921";
          ring-color = "fabd2f";
          inside-color = "d79921";
          key-hl-color = "eb6f92";
          separator-color = "00000000";
          text-color = "ebdbb2";
          text-caps-lock-color = "";
          line-ver-color = "eb6f92";
          ring-ver-color = "eb6f92";
          inside-ver-color = "d79921";
          text-ver-color = "ebdbb2";
          ring-wrong-color = "d79921";
          text-wrong-color = "d79921";
          inside-wrong-color = "d79921";
          inside-clear-color = "d79921";
          text-clear-color = "ebdbb2";
          ring-clear-color = "9ccfd8";
          line-clear-color = "d79921";
          line-wrong-color = "d79921";
          bs-hl-color = "d79921";
          grace = 2;
          grace-no-mouse = true;
          grace-no-touch = true;
          fade-in = "0.1";
          ignore-empty-password = true;
        };
      };
      xdg.configFile = {
        # This is from the hyprland home-manager module
        # https://github.com/hyprwm/Hyprland/blob/main/nix/hm-module.nix
        # We need it here to ensure that all our other gui services (waybar, dynamic-wallpaper, etc) are started
        # when we login.
        # Also we get a nice hyprland reload when the config changes
        "hypr/hyprland.conf" = {
          text =
            ''
              exec-once=${pkgs.dbus}/bin/dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY HYPRLAND_INSTANCE_SIGNATURE XDG_CURRENT_DESKTOP XAUTHORITY DISPLAY && systemctl --user start hyprland-session.target
            ''
            #+ ''
            #  plugin = ${inputs.hy3.packages.x86_64-linux.hy3}/lib/libhy3.so
            #''
            #+ ''
            #  plugin = ${inputs.hyprNStack.packages.${pkgs.system}.hyprNStack}/lib/libhyprNStack.so
            #''
            + builtins.readFile ./configs/hyprland.conf;
          onChange = ''
            (  # execute in subshell so that `shopt` won't affect other scripts
               shopt -s nullglob  # so that nothing is done if /tmp/hypr/ does not exist or is empty
               for instance in /tmp/hypr/*; do
                 HYPRLAND_INSTANCE_SIGNATURE=''${instance##*/} ${config.programs.hyprland.package}/bin/hyprctl reload config-only \
                   || true  # ignore dead instance(s)
               done
             )
          '';
        };

        "hypr/wallpaper" = {
          source = ./configs/wallpaper;
          recursive = true;
          force = true;
        };

        "waybar" = {
          source = ./configs/waybar;
          recursive = true;
          onChange = ''systemctl --user restart waybar'';
        };

        "rofi/config.rasi" = {
          source = ./configs/rofi/config.rasi;
          recursive = true;
        };

        "rofi/theme.rasi" = {
          source = ./configs/rofi/theme.rasi;
          recursive = true;
        };
      };
    };
  };
}
