{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.wayland;
in
{
  options.modules.desktop.wayland = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    services.flatpak.enable = true;
    programs.dconf.enable = true;
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
        wtype # handy (speech to text) uses this for clipboard access
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
        waytray
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
    myhm = {
      # begin: disabled questionable section
      #home.sessionVariables = {
      #  QT_QPA_PLATFORM = "wayland";
      #  XDG_SESSION_TYPE = "wayland";
      #  NIXOS_OZONE_WL = "1";
      #  MOZ_ENABLE_WAYLAND = "1";
      #  _JAVA_AWT_WM_NONREPARENTING = "1";
      #};
      # end: disable questionanable section

      #home.packages = with pkgs; [
      #  qt5.qtwayland
      #  qt6.qtwayland

      #  wl-clipboard # wl-{copy,paste}
      #  wtype # virtual keystroke insertion

      #  # imv # image viewer
      #  oculante # image viewer (rust)
      #  grim # area selection
      #  slurp # screen capture
      #  wf-recorder # screen record
      #  wev # event viewer
      #];
    };
  };
}
