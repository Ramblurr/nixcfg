{
  inputs,
  pkgs,
  config,
  lib,
  ...
}:
{
  services = {
    dbus.enable = true;
    gvfs.enable = true; # Needed for nautilus
  };
  programs = {
    kdeconnect.enable = true; # Connect phone to PC
    hyprland.enable = true;
    hyprland.package = inputs.hyprland.packages.${pkgs.system}.hyprland;
    #hyprland.nvidiaPatches = true;
    hyprland.enableNvidiaPatches = true;
  };
  security.polkit.enable = true;
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
      inputs.hyprNStack.packages.${pkgs.system}.hyprNStack
      inputs.hy3.packages.x86_64-linux.hy3
    ];
  };
}
