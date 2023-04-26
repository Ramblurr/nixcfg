{
  pkgs,
  config,
  lib,
  ...
}: {
  services.xserver.desktopManager.plasma5.enable = true;

  home-manager.users.ramblurr = {pkgs, ...} @ hm: {
    home.persistence."/persist/home/ramblurr" = {
      directories = [
        ".config/gtk-3.0" # fuse mounted from /nix/dotfiles/Plasma/.config/gtk-3.0
        ".config/gtk-4.0" # to /home/$USERNAME/.config/gtk-3.0
        ".config/KDE"
        ".config/kde.org"
        ".config/plasma-workspace"
        ".config/xsettingsd"
        ".local/share/kscreen"
        ".local/share/kwalletd"
        ".local/share/baloo"
        ".local/share/kactivitymanagerd"
        ".local/share/sddm"
        ".kde"
      ];
      files = [
        ".config/akregatorrc"
        ".config/baloofileinformationrc"
        ".config/baloofilerc"
        ".config/bluedevilglobalrc"
        ".config/device_automounter_kcmrc"
        ".config/dolphinrc"
        ".config/filetypesrc"
        ".config/gtkrc"
        ".config/gtkrc-2.0"
        ".config/gwenviewrc"
        ".config/kactivitymanagerd-pluginsrc"
        ".config/kactivitymanagerd-statsrc"
        ".config/kactivitymanagerd-switcher"
        ".config/kactivitymanagerdrc"
        ".config/katemetainfos"
        ".config/katerc"
        ".config/kateschemarc"
        ".config/katevirc"
        ".config/kcmfonts"
        ".config/kcminputrc"
        ".config/kconf_updaterc"
        ".config/kded5rc"
        ".config/kdeglobals"
        ".config/kgammarc"
        ".config/kglobalshortcutsrc"
        ".config/khotkeysrc"
        ".config/kmixrc"
        ".config/konsolerc"
        ".config/kscreenlockerrc"
        ".config/ksmserverrc"
        ".config/ksplashrc"
        ".config/ktimezonedrc"
        ".config/kwinrc"
        ".config/kwinrulesrc"
        ".config/kxkbrc"
        ".config/mimeapps.list"
        ".config/partitionmanagerrc"
        ".config/plasma-localerc"
        ".config/plasma-nm"
        ".config/plasma-org.kde.plasma.desktop-appletsrc"
        ".config/plasmanotifyrc"
        ".config/plasmarc"
        ".config/plasmashellrc"
        ".config/PlasmaUserFeedback"
        ".config/plasmawindowed-appletsrc"
        ".config/plasmawindowedrc"
        ".config/powermanagementprofilesrc"
        ".config/spectaclerc"
        ".config/startkderc"
        ".config/systemsettingsrc"
        #".config/Trolltech.conf"
        ".config/user-dirs.locale"
        ".local/share/krunnerstaterc"
        #".local/share/user-places.xbel"
        #".local/share/user-places.xbel.bak"
        #".local/share/user-places.xbel.tbcache"
      ];
    };
  };
}
