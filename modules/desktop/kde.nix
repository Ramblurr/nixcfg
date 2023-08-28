{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.kde;
  username = config.modules.users.primaryUser.username;
in {
  options.modules.desktop.kde = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    services.xserver.desktopManager.plasma5.enable = true;
    services.xserver.desktopManager.plasma5.runUsingSystemd = false;

    home-manager.users."${username}" = {pkgs, ...} @ hm: {
      home.packages = [
        pkgs.digikam
        pkgs.libsForQt5.kfind
        pkgs.krename
      ];
      xdg.configFile."baloofilerc".text = ''
        [Basic Settings]
        Indexing-Enabled=true

        [General]
        exclude filters=*~,*.part,*.o,*.la,*.lo,*.loT,*.moc,moc_*.cpp,qrc_*.cpp,ui_*.h,cmake_install.cmake,CMakeCache.txt,CTestTestfile.cmake,libtool,config.status,confdefs.h,autom4te,conftest,confstat,Makefile.am,*.gcode,.ninja_deps,.ninja_log,build.ninja,*.csproj,*.m4,*.rej,*.gmo,*.pc,*.omf,*.aux,*.tmp,*.po,*.vm*,*.nvram,*.rcore,*.swp,*.swap,lzo,litmain.sh,*.orig,.histfile.*,.xsession-errors*,*.map,*.so,*.a,*.db,*.qrc,*.ini,*.init,*.img,*.vdi,*.vbox*,vbox.log,*.qcow2,*.vmdk,*.vhd,*.vhdx,*.sql,*.sql.gz,*.ytdl,*.class,*.pyc,*.pyo,*.elc,*.qmlc,*.jsc,*.fastq,*.fq,*.gb,*.fasta,*.fna,*.gbff,*.faa,po,CVS,.svn,.git,_darcs,.bzr,.hg,CMakeFiles,CMakeTmp,CMakeTmpQmake,.moc,.obj,.pch,.uic,.npm,.yarn,.yarn-cache,__pycache__,node_modules,node_packages,nbproject,core-dumps,lost+found,dist,target,build,.terraform,.terragrunt,.gradle,.m2,logs,.lsp,output,out,*.crdownload,*.part,._*,*.vim*tmp,_lock,_cacache,.cache,.cpcache,.deps,.pioenvs,.piolibdeps,.esphome,stable-diffusion-ui,ovos-buildroot,datomic.data,datomic.data*,redis,redis-dev,.Trash,venv
        exclude filters version=8
      '';

      home.persistence."/persist/home/${username}" = mkIf config.modules.impermanence.enable {
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
          ".local/share/gwenview"
          ".local/share/dolphin"
          ".local/share/okular"
          ".kde"
          ".local/share/digikam"
          ".cache/digikam"
        ];
        files = [
          ".config/digikamrc"
          ".config/digikam_systemrc"
          ".config/akregatorrc"
          ".config/baloofileinformationrc"
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
          ".local/share/user-places.xbel"
          #".local/share/user-places.xbel.bak"
          #".local/share/user-places.xbel.tbcache"
        ];
      };
    };
  };
}
