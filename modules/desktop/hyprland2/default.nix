{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.hyprland2;
  username = config.modules.users.primaryUser.username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.hyprland2 = {
    enable = lib.mkEnableOption "Enable Hyprland";
    ags-config = lib.mkOption {
      type = lib.types.package;
      default = pkgs.callPackage ../../../configs/ags { inherit inputs; };
    };
  };

  config = mkIf cfg.enable {
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
    services.xserver.displayManager.startx.enable = true;

    programs.hyprland = {
      enable = true;
      package = inputs.hyprland.packages.${pkgs.system}.hyprland;
      xwayland.enable = true;
    };

    xdg.portal = {
      enable = true;
      extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
    };

    security = {
      polkit.enable = true;
      pam.services.ags = { };
    };

    services.dbus.packages = [ pkgs.gnome.gnome-disk-utility ];
    environment.systemPackages =
      with pkgs;
      with gnome;
      [
        gnome-disk-utility
        gnome.adwaita-icon-theme
        loupe
        adwaita-icon-theme
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

    services.greetd = {
      enable = true;
      settings.default_session.command = pkgs.writeShellScript "greeter" ''
        export XKB_DEFAULT_LAYOUT=${config.services.xserver.xkb.layout}
        export XCURSOR_THEME=Adwaita
        ${cfg.ags-config}/bin/greeter
      '';
    };

    systemd.tmpfiles.rules =
      if withImpermanence then
        [ "d '/var/cache/greeter' - greeter greeter - -" ]
      else
        [ "d '/persist/var/cache/greeter' - greeter greeter - -" ];

    environment.persistence."/persist" = lib.mkIf withImpermanence {
      directories = [ "/var/cache/greeter" ];
    };

    #system.activationScripts.wallpaper = ''
    #  PATH=$PATH:${pkgs.busybox}/bin:${pkgs.jq}/bin
    #  CACHE="/var/cache/greeter"
    #  OPTS="$CACHE/options.json"
    #  HOME="/home/$(find /home -maxdepth 1 -printf '%f\n' | tail -n 1)"

    #  cp $HOME/.cache/ags/options.json $OPTS
    #  chown greeter:greeter $OPTS

    #  BG=$(cat $OPTS | jq -r '.wallpaper // "$HOME/.config/background"')
    #  cp $BG $CACHE/background
    #  chown greeter:greeter $CACHE/background
    #'';
  };
}
