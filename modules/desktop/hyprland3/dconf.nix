{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.desktop.hyprland3;
in

{
  config = lib.mkIf cfg.enable {

    myhm =
      { lib, ... }@hm:
      with lib.hm.gvariant;
      {
        dconf.settings = {
          "org/virt-manager/virt-manager/connections" = {
            autoconnect = [ "qemu:///system" ];
            uris = [
              "qemu:///system"
              "qemu+ssh://ramblurr@debord/system"
            ];
          };
          "org/virt-manager/virt-manager/confirm" = {
            forcepoweroff = true;
            removedev = false;
            unapplied-dev = true;
          };
          "org/virt-manager/virt-manager/details" = {
            show-toolbar = true;
          };

          "org/virt-manager/virt-manager/new-vm" = {
            graphics-type = "system";
          };

          "org/virt-manager/virt-manager/vmlist-fields" = {
            disk-usage = false;
            network-traffic = false;
          };

          "org/virt-manager/virt-manager/vms/4648adfb536a41818944209d6cff0c31" = {
            autoconnect = 1;
            scaling = 1;
            vm-window-size = mkTuple [
              1688
              703
            ];
          };
          "org/virt-manager/virt-manager/vms/e5c7ddccfa054ae09b966cb8e5176436" = {
            autoconnect = 1;
            vm-window-size = mkTuple [
              1688
              1398
            ];
          };
          "org/virt-manager/virt-manager/vms/f4f133d6079340459df7c38ff8b71cf8" = {
            autoconnect = 1;
            scaling = 1;
            vm-window-size = mkTuple [
              1688
              1398
            ];
          };
          "org/gnome/desktop/input-sources" = {
            sources = [
              (mkTuple [
                "xkb"
                "hu"
              ])
            ];
            xkb-options = [ "terminate:ctrl_alt_bksp" ];
          };

          "org/gnome/desktop/interface" = {
            show-battery-percentage = true;
          };

          "org/gnome/desktop/peripherals/touchpad" = {
            tap-to-click = true;
            two-finger-scrolling-enabled = true;
          };

          "org/gnome/desktop/search-providers" = {
            disabled = [ "org.gnome.Boxes.desktop" ];
            enabled = [ "org.gnome.Weather.desktop" ];
            sort-order = [
              "org.gnome.Contacts.desktop"
              "org.gnome.Documents.desktop"
              "org.gnome.Nautilus.desktop"
              "org.gnome.Calendar.desktop"
              "org.gnome.Calculator.desktop"
              "org.gnome.Software.desktop"
              "org.gnome.Settings.desktop"
              "org.gnome.clocks.desktop"
              "org.gnome.design.IconLibrary.desktop"
              "org.gnome.seahorse.Application.desktop"
              "org.gnome.Weather.desktop"
              "org.gnome.Boxes.desktop"
            ];
          };

          "org/gnome/desktop/session" = {
            idle-delay = mkUint32 0;
          };

          "org/gnome/desktop/wm/keybindings" = {
            close = [ "<Alt>q" ];
            move-to-workspace-1 = [ "<Shift><Super>1" ];
            move-to-workspace-2 = [ "<Shift><Super>2" ];
            move-to-workspace-3 = [ "<Shift><Super>3" ];
            move-to-workspace-4 = [ "<Shift><Super>4" ];
            move-to-workspace-5 = [ "<Shift><Super>5" ];
            switch-to-workspace-1 = [ "<Super>1" ];
            switch-to-workspace-2 = [ "<Super>2" ];
            switch-to-workspace-3 = [ "<Super>3" ];
            switch-to-workspace-4 = [ "<Super>4" ];
            switch-to-workspace-5 = [ "<Super>5" ];
            toggle-fullscreen = [ "<Super>g" ];
          };

          "org/gnome/shell/keybindings" = {
            switch-to-application-1 = [ ];
            switch-to-application-2 = [ ];
            switch-to-application-3 = [ ];
            switch-to-application-4 = [ ];
            switch-to-application-5 = [ ];
          };

          "org/gnome/desktop/wm/preferences" = {
            mouse-button-modifier = "<Super>";
            num-workspaces = 5;
            resize-with-right-button = true;
            focus-mode = "sloppy";
          };

          "org/gnome/mutter" = {
            dynamic-workspaces = false;
            edge-tiling = true;
            num-workspaces = 5;
            workspaces-only-on-primary = true;
          };

          "org/gnome/settings-daemon/plugins/media-keys" = {
            custom-keybindings = [
              "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/"
            ];
            mic-mute = [ "AudioMicMute" ];
            next = [ "AudioNext" ];
            play = [ "AudioPlay" ];
            previous = [ "AudioPrev" ];
            stop = [ "AudioStop" ];
            volume-down = [ "AudioLowerVolume" ];
            volume-up = [ "AudioRaiseVolume" ];

            home = [ "<Super>e" ];
            www = [ "<Super>w" ];
          };

          "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
            binding = "<Super>Return";
            command = "xterm";
            name = "term";
          };

          "org/gnome/settings-daemon/plugins/power" = {
            idle-dim = false;
            power-button-action = "interactive";
            sleep-inactive-ac-type = "nothing";
            sleep-inactive-battery-type = "nothing";
          };

          "org/gnome/shell" = {
            favorite-apps = [
              "firefox.desktop"
              "kitty.desktop"
              "org.gnome.Nautilus.desktop"
              "logseq.desktop"
              "discord.desktop"
              "com.usebottles.bottles.desktop"
              "org.gnome.Software.desktop"
            ];
          };

          "org/gnome/shell/app-switcher" = {
            current-workspace-only = false;
          };

          "org/gnome/shell/keybindings" = {
            toggle-application-view = [ "<Super>r" ];
          };

          "system/locale" = {
            region = "hu_HU.UTF-8";
          };

          "com/github/stunkymonkey/nautilus-open-any-terminal" = {
            terminal = "kitty";
          };

          "org/gnome/TextEditor" = {
            keybindings = "vim";
          };
        };
      };
  };
}
