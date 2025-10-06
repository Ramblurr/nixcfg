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
      xwayland.enable = true;
    };

    xdg.portal = {
      enable = true;
      extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
      xdgOpenUsePortal = true;
      #config = {
      #  common = {
      #    #    default = "gtk";
      #    "org.freedesktop.impl.portal.Screencast" = "hyprland";
      #    "org.freedesktop.impl.portal.Screenshot" = "hyprland";
      #  };
      #};
    };

    environment.systemPackages =
      with pkgs;
      #with gnome;
      [
        gnome-disk-utility
        loupe # gui image viewer
        nautilus
        baobab
        wl-gammactl
        wl-clipboard
        pavucontrol
        brightnessctl
        swww
        wlr-randr
        swhkd
        wf-recorder # screen record cli
        wev # event viewer
        swappy # screenshot annotate
        grimblast # select screen area + screenshot in one
        ksnip
        # flameshot it great but has very poor wayland support
        # ref: https://github.com/flameshot-org/flameshot/issues/3757
        # (flameshot.override {
        #   enableWlrSupport = true;
        # })
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
    };
  };
}
