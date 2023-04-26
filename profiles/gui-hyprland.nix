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
    hyprland.enable = true;
    hyprland.nvidiaPatches = true;
  };
  security.polkit.enable = true;
  environment = {
    systemPackages = with pkgs; [
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
      networkmanagerapplet # Network manager tray icon
      pavucontrol # Sound manager
      polkit_gnome # Polkit manager
      rofi-wayland # App launcher
      slurp # Monitor selector
      swappy # Edit screenshots
      wdisplays # Displays manager
      wl-clipboard # Clipboard daemon
      hyprpicker # color picker
      gtk-layer-shell # shell components on wayland
    ];

    etc = {
      #"wlogout-icons".source = "${pkgs.wlogout}/share/wlogout/icons";
      #"polkit-gnome".source = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
    };
  };
}
