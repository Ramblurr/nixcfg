{
  pkgs,
  config,
  lib,
  ...
}: {
  services = {
    dbus.enable = true;
    gvfs.enable = true; # Needed for nautilus
  };
  programs = {
    kdeconnect.enable = true; # Connect phone to PC
    hyperland.enable = true;
  };

  environment = {
    systemPackages = with pkgs; [
      # Status bar
      (waybar.overrideAttrs (oldAttrs: {
        mesonFlags = oldAttrs.mesonFlags ++ ["-Dexperimental=true"];
        postPatch = ''
          sed -i 's/zext_workspace_handle_v1_activate(workspace_handle_);/const std::string command = "hyprctl dispatch workspace " + name_;\n\tsystem(command.c_str());/g' src/modules/wlr/workspace_manager.cpp
        '';
      }))
      baobab # Disk usage analyser
      blueberry # Bluetooth manager
      clipman # Clipboard manager for wayland
      dunst # Notification daemon
      gnome.file-roller # Archive file manager
      gnome.gnome-calculator # Calculator
      gnome.gnome-disk-utility # Disks manager
      gnome.gnome-themes-extra # Adwaita GTK theme
      gnome.nautilus # File manager
      grim # Screenshot tool
      jq # JSON parser
      networkmanagerapplet # Network manager tray icon
      pavucontrol # Sound manager
      polkit_gnome # Polkit manager
      rofi-wayland # App launcher
      slurp # Monitor selector
      swappy # Edit screenshots
      wdisplays # Displays manager
      wl-clipboard # Clipboard daemon
      wlogout # Logout screen
    ];

    etc = {
      "wlogout-icons".source = "${pkgs.wlogout}/share/wlogout/icons";
      "polkit-gnome".source = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
    };
  };

  security.polkit.enable = true;
}
