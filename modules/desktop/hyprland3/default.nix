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

    security = {
      polkit.enable = true;
      # unlock keyring on login
      pam.services.greetd.enableGnomeKeyring = true;
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
      #displayManager.enable = false;
      #displayManager.sddm.wayland.enable = false;
      #displayManager.sddm.enable = false;
      #displayManager.ly.enable = false;
      greetd = {
        enable = true;
        settings = rec {
          terminal = {
            vt = lib.mkForce 7;
          };
          tuigreet_session =
            let
              session = "${pkgs.hyprland}/bin/Hyprland";
              tuigreet = "${lib.getExe pkgs.tuigreet}";
              opts = [
                "--time"
                "--remember"
                "--theme"
                "border=lightblue;prompt=green;time=orange;button=yellow;container=black"
                "--power-shutdown"
                "systemctl poweroff -i"
                "--power-reboot"
                "systemctl reboot -i"
                "--cmd"
                "${session}"
              ];
            in
            {
              command = "${tuigreet} ${lib.strings.escapeShellArgs opts}";
              user = "greeter";
            };
          default_session = tuigreet_session;
        };
      };
    };
    systemd.tmpfiles.rules = lib.mkIf withImpermanence [
      "d /persist/var/cache/greeter - greeter greeter - -"
    ];
    environment.persistence."/persist".directories = lib.mkIf withImpermanence [ "/var/cache/greeter" ];
    # allow greeter to poweroff and reboot
    security.polkit.extraConfig = ''
       polkit.addRule(function(action, subject) {
      if (
        subject.user == "greeter"
          && (
            action.id == "org.freedesktop.login1.reboot" ||
            action.id == "org.freedesktop.login1.reboot-multiple-sessions" ||
            action.id == "org.freedesktop.login1.power-off" ||
            action.id == "org.freedesktop.login1.power-off-multiple-sessions"
          )
        )
      {
        return polkit.Result.YES;
      }
       })
    '';
  };
}
