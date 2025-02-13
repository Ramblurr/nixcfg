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
  hyprland-emacs = pkgs.writeScriptBin "hyprland-emacs" ''
    if [[ $(hyprctl activewindow -j | jq -r .class) == "emacs" ]]; then
        command="(my/emacs-hypr-integration \"$@\")"
        #echo "emacs -> $command" >> ~/emacs-wm.log
        emacsclient -e "$command" >> ~/emacs-wm.log
    else
        #echo "hypr -> $@" >> ~/emacs-wm.log
        hyprctl dispatch "$@"
    fi

  '';

in

{
  config = lib.mkIf cfg.enable {
    environment.persistence."/persist" = {
      users.${username} = {

        files = [
          ".config/hypr/+bindings.conf"
        ];
      };
    };
    myhm =
      { ... }@hm:
      {
        xdg.desktopEntries."org.gnome.Settings" = {
          name = "Settings";
          comment = "Gnome Control Center";
          icon = "org.gnome.Settings";
          exec = "env XDG_CURRENT_DESKTOP=gnome ${pkgs.gnome-control-center}/bin/gnome-control-center";
          categories = [ "X-Preferences" ];
          terminal = false;
        };

        services.swayidle = {
          enable = true;
          timeouts = [
            {
              timeout = 549;
              command = ''${pkgs.libnotify}/bin/notify-send -t 15000 -u critical -i /etc/profiles/per-user/${username}/share/icons/Adwaita/symbolic/status/system-lock-screen-symbolic.svg  "Idle timeout" "Screen is locking in 1 minute"'';
            }
            {
              timeout = 550;
              command = "${hyprland}/bin/hyprctl dispatch dpms off";
              resumeCommand = "${hyprland}/bin/hyprctl dispatch dpms on";
            }
          ];
        };

        wayland.windowManager.hyprland = {
          enable = true;
          package = hyprland;
          systemd.enable = true;
          xwayland.enable = true;
          #extraConfig = builtins.readFile ../hyprland/configs/hyprland.conf;
          # plugins = [ inputs.hy3.packages.x86_64-linux.hy3 ];
          settings = {
            debug = {
              disable_logs = true;
            };
            exec-once = [
              #"swhks &"
              #"ags -b hypr"
              #"hyprctl setcursor Adwaita 24"
              #"easyeffects --gapplication-service"
            ];
            monitor = [
              "HDMI-A-1,3840x2160,0x0,1,transform,1"
              "DP-2,highres,2160x1250,1"
              ",addreserved,0,0,0,0"
            ];
            general = {
              layout = "master";
              resize_on_border = true;
            };
            cursor = {
              no_warps = true;
            };
            misc = {
              disable_splash_rendering = true;
              force_default_wallpaper = 1;
            };
            input = {
              kb_layout = "us";
              follow_mouse = 1;
              touchpad = {
                natural_scroll = "no";
                disable_while_typing = true;
                drag_lock = true;
              };
              sensitivity = 0.2;
              float_switch_override_focus = 2;
              force_no_accel = true;
            };
            cursor = {
              # this is an nvidia workaround
              # https://github.com/hyprwm/Hyprland/issues/4523
              no_hardware_cursors = true;
            };
            binds = {
              workspace_back_and_forth = true;
              allow_workspace_cycles = true;
            };
            dwindle = {
              pseudotile = "yes";
              preserve_split = "yes";
              # no_gaps_when_only = "yes";
            };
            gestures = {
              workspace_swipe = false;
            };
            workspace = [
              "m[HDMI-A-1], layoutopt:orientation:bottom"
              "m[HDMI-A-1], layoutopt:slave_count_for_center_master:5"
              "m[HDMI-A-1], layoutopt:allow_small_split:true"
              "m[HDMI-A-1], layoutopt:mfact:0.33"
              "1,monitor:DP-2"
              "2,monitor:DP-2"
              "3,monitor:DP-2"
              "4,monitor:DP-2"
              "5,monitor:DP-2"
              "6,monitor:HDMI-A-1"
              "7,monitor:DP-2"
              "8,monitor:DP-2"
              "9,monitor:DP-2"
              "11,monitor:HDMI-A-1"
              "12,monitor:HDMI-A-1"
              "13,monitor:HDMI-A-1"
              "21,monitor:HDMI-A-1"
            ];
            windowrule =
              let
                f = regex: "float, ^(${regex})$";
              in
              [
                (f "org.gnome.Calculator")
                (f "org.gnome.Nautilus")
                (f "pavucontrol")
                (f "nm-connection-editor")
                (f "blueberry.py")
                (f "org.gnome.Settings")
                (f "org.gnome.design.Palette")
                (f "Color Picker")
                (f "xdg-desktop-portal")
                (f "xdg-desktop-portal-gnome")
                #(f "com.github.Aylur.ags")
                "workspace 2 silent,class:^(firefox-personal)$"
                "workspace 4 silent,class:^(firefox-work)$"
                "workspace 2 silent,class:^(codium-url-handler)$"
                "workspace 11 silent,class:^(Signal|Slack|Microsoft Teams|Element|fluffychat)$"
                "workspace 6 silent,class:^(Steam)$"
                "workspace 6 silent,class:^(ProtoEvo)$"
                "size 1920 1080,class:^(ProtoEvo)$"
                "float,class:^(ProtoEvo)$"
                "center,class:^(ProtoEvo)$"
                "workspace 6 silent,title:(.*Steam.*)"
                "workspace 3 silent,class:(steam_app_.*)$"
                "workspace 1 silent,class:^(startup-kitty)$"
                "workspace 12 silent,class:^(thunderbird-personal)$"
                "workspace 12 silent,class:^(thunderbird-work)$"
                "workspace 13 silent,class:^(Morgen)$"
                "workspace 3 silent,class:^(startup-monitor)$"
                "workspace 6 silent,class:(steam_app_1245620)$" # Elden Ring must be played on non-widescreen monitor
                "tile,class:^(Godot.*)$"
                "float,class:^(Steam)$,title:(.*Steam.*)"
                "float,class:^(Steam)$"
                "float,class:^(steam_app_.*)$"
                "tile,class:^(photoshop.exe)$"
                "float,title:^(.* — Sharing Indicator)$"
                "size 0 0,title:^(.* — Sharing Indicator)$"
                "float,title:^(Wine System Tray)$"
                "size 0 0,title:^(Wine System Tray)$"
                "float,title:^(MyAI)$"
                "size 1600 1000,title:^(MyAI)$"
                "monitor DP-2,title:^(MyAI)$"
                "move 0 0,title:^(MyAI)$"
                "tile,class:^(Mullvad VPN)$"
                "nofocus,class:^jetbrains-(?!toolbox),floating:1,title:^win\d+$"
                "center, class:^(.*jetbrains.*)$, title:^(Confirm Exit|Open Project|win424|win201|splash)$"
                "size 640 400, class:^(.*jetbrains.*)$, title:^(splash)$"
                "nofocus,class:^jetbrains-(?!toolbox),floating:1,title:^win\d+$"
                #"forceinput, class:^jetbrains-(?!toolbox), floating:1, title:^(?!win)(.*)"
                "monitor HDMI-A-1,title:^(Roon)$"
                "workspace 6 silent,title:^(Roon)$"
                "size 2633 1843,title:^(Roon)$"
                "move 634 155,title:^(Roon)$"
              ];
            bind =
              let
                binding =
                  mod: cmd: key: arg:
                  "${mod}, ${key}, ${cmd}, ${arg}";

                binding-emacs =
                  mod: cmd: key: arg:
                  binding mod "exec" key "${hyprland-emacs}/bin/hyprland-emacs ${cmd} ${arg}";

                #mvfocus = binding "SUPER" "movefocus";
                mvfocus =
                  (if config.modules.editors.emacs.enable then binding-emacs else binding) "SUPER"
                    "movefocus";
                #mvwin = binding "SUPER SHIFT" "movewindow";
                mvwin =
                  (if config.modules.editors.emacs.enable then binding-emacs else binding) "SUPER SHIFT"
                    "movewindow";
                ws = binding "SUPER" "workspace";
                resizeactive = binding "SUPER CTRL" "resizeactive";
                mvactive = binding "SUPER ALT" "moveactive";
                mvtows = binding "SUPER SHIFT" "movetoworkspacesilent";
                #e = "exec, ags -b hypr";
                arr = [
                  1
                  2
                  3
                  4
                  5
                  6
                  7
                  8
                  9
                  10
                  11
                  12
                  13
                  14
                  15
                  16
                  17
                  18
                  19
                  20
                  21
                  22
                ];
                expandArr =
                  fn:
                  map (
                    i:
                    fn (
                      if (i == 10) then
                        "0"
                      else if (i - 10 > 0) then
                        "F${toString (i - 10)}"
                      else
                        (toString i)
                    ) (toString i)
                  ) arr;
              in
              [
                "SUPER, D, exec, anyrun"
                ''SUPER SHIFT,Z, exec, grimblast --freeze save area - | swappy -f - -o ~/docs/img/screenshots/scrn-$(date +"%Y-%m-%d-%H-%M-%S").png''
                "SUPER, Return, exec, kitty"
                "SUPER, F, fullscreen"
                "SUPER, M, fullscreenstate, -1 2"
                "SUPER SHIFT, Q, killactive"
                "SUPER, B, togglefloating"
                # Move focus with mod key + hjkl
                (mvfocus "L" "r")
                (mvfocus "H" "l")
                (mvfocus "J" "d")
                (mvfocus "K" "u")
                # Move window with SHIFT + MOD + hjkl
                (mvwin "L" "r")
                (mvwin "H" "l")
                (mvwin "J" "d")
                (mvwin "K" "u")
              ]
              # Switch workspaces with mod key + [0-9]
              ++ (expandArr ws)
              # Move active window to a workspace with mod key + SHIFT + [0-9]
              ++ (expandArr mvtows);
            bindm = [

              # Move/resize windows with mod key + LMB/RMB and dragging
              "SUPER, mouse:273, resizewindow"
              "SUPER, mouse:272, movewindow"
            ];

            master = {
              allow_small_split = true;
              mfact = 0.33;
              slave_count_for_center_master = 0;
              orientation = "center";
            };

            animations = {
              enabled = "yes";
              bezier = "myBezier, 0.05, 0.9, 0.1, 1.05";
              animation = [
                "windows, 1, 5, myBezier"
                "windowsOut, 1, 7, default, popin 80%"
                "border, 1, 10, default"
                "fade, 1, 7, default"
                "workspaces, 1, 6, default"
              ];
            };

            decoration = {
              shadow = {
                enabled = true;
                render_power = 2;
                range = 8;
                color = "rgba(00000044)";
              };

              dim_inactive = false;

              blur = {
                enabled = true;
                size = 8;
                passes = 3;
                new_optimizations = "on";
                noise = 1.0e-2;
                contrast = 0.9;
                brightness = 0.8;
                popups = true;
              };
            };

            source = "~/.config/hypr/+bindings.conf";
            # plugins = { };
          }; # end settings
        };
      };
  };
}
