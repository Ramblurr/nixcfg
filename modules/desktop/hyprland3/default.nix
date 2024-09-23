{
  options,
  config,
  lib,
  pkgs,

  inputs,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.hyprland3;
  username = config.modules.users.primaryUser.username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.hyprland3 = {
    enable = lib.mkEnableOption "Enable Hyprland";
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.enable && config.modules.desktop.kde != true;
        message = "My hyprland config is mutually exclusive with KDE Plasma";
      }
    ];
    modules.desktop.wayland.enable = false;
    # dconf
    programs.dconf.enable = true;

    services = {
      xserver = {
        enable = true;
        excludePackages = [ pkgs.xterm ];
      };
      printing.enable = true;
      flatpak.enable = true;
    };

    ## upstream
    #services.xserver.displayManager.startx.enable = true;

    programs.hyprland = {
      enable = true;
      package = inputs.hyprland.packages.${pkgs.system}.hyprland;
      xwayland.enable = true;
    };

    xdg.portal = {
      enable = true;
      extraPortals = with pkgs; [
        # hyprland module enables its own portal
        #libsForQt5.xdg-desktop-portal-kde
        xdg-desktop-portal-gtk
      ];
      xdgOpenUsePortal = true;
      config = {
        common = {
          default = "gtk";
          "org.freedesktop.impl.portal.Screencast" = "hyprland";
          "org.freedesktop.impl.portal.Screenshot" = "hyprland";
        };
      };
    };

    security = {
      polkit.enable = true;
      # unlock keyring on login
      pam.services.greetd.enableGnomeKeyring = true;
    };

    environment.systemPackages =
      with pkgs;
      #with gnome;
      [
        morewaita-icon-theme
        adwaita-icon-theme
        qogir-icon-theme
        gnome-disk-utility
        loupe
        nautilus
        baobab

        gnome-text-editor
        gnome-calendar
        gnome-boxes
        gnome-system-monitor
        gnome-control-center
        gnome-weather
        gnome-calculator
        gnome-clocks
        gnome-software # for flatpak
        wl-gammactl
        wl-clipboard
        wayshot
        pavucontrol
        brightnessctl
        swww
        wlr-randr
        inputs.anyrun.packages.${system}.anyrun
      ];

    systemd = {
      user.services.polkit-gnome-authentication-agent-1 = {
        description = "polkit-gnome-authentication-agent-1";
        wantedBy = [ "graphical-session.target" ];
        wants = [ "graphical-session.target" ];
        after = [ "graphical-session.target" ];
        serviceConfig = {
          Type = "simple";
          ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutStopSec = 10;
        };
      };
    };

    services = {
      dbus.packages = [ pkgs.gnome-disk-utility ];
      gvfs.enable = true;
      devmon.enable = true;
      udisks2.enable = true;
      upower.enable = true;
      power-profiles-daemon.enable = true;
      accounts-daemon.enable = true;
      gnome = {
        evolution-data-server.enable = true;
        glib-networking.enable = true;
        gnome-keyring.enable = true;
        gnome-online-accounts.enable = true;
      };
      displayManager.sddm.wayland.enable = true;
      displayManager.enable = true;
      displayManager.sddm.enable = true;
    };

  };
}
