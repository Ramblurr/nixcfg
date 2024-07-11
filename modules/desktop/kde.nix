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
  cfg = config.modules.desktop.kde;
  username = config.modules.users.primaryUser.username;
  withImpermanence = config.modules.impermanence.enable;

  nerdfonts = (
    pkgs.nerdfonts.override {
      fonts = [
        #"Noto Sans"
        "Iosevka"
        "FiraCode"
        "Mononoki"
        "JetBrainsMono"
        "NerdFontsSymbolsOnly"
      ];
    }
  );

  theme = {
    name = "adw-gtk3-dark";
    package = pkgs.adw-gtk3;
  };
  font = {
    name = "Noto Nerd Font";
    package = nerdfonts;
  };
  cursorTheme = {
    name = "Adwaita";
    size = 24;
    package = pkgs.gnome.adwaita-icon-theme;
  };
  iconTheme = {
    name = "Adwaita";
    package = pkgs.gnome.adwaita-icon-theme;
  };
in
{
  options.modules.desktop.kde = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    services.desktopManager.plasma6.enable = true;

    fonts = {

      packages = with pkgs; [
        cantarell-fonts
        font-awesome
        theme.package
        font.package
        cursorTheme.package
        iconTheme.package
        gnome.adwaita-icon-theme
        papirus-icon-theme
        iosevka-comfy.comfy-fixed
        noto-fonts-emoji
        noto-fonts
        font-awesome
        liberation_ttf # free corefonts-metric-compatible replacement
        ttf_bitstream_vera
        gelasio # metric-compatible with Georgia
        powerline-symbols
      ];

      fontDir.enable = true;

      fontconfig = {
        defaultFonts = {
          serif = [ "Noto Serif" ];
          sansSerif = [ "Noto Sans" ];
          monospace = [ "Iosevka Comfy Fixed" ];
          emoji = [ "Noto Color Emoji" ];
        };
      };
    };

    home-manager.users."${username}" =
      { pkgs, ... }@hm:
      {
        xdg.configFile."baloofilerc".text = ''
          [Basic Settings]
          Indexing-Enabled=true

          [General]
          exclude filters=*~,*.part,*.o,*.la,*.lo,*.loT,*.moc,moc_*.cpp,qrc_*.cpp,ui_*.h,cmake_install.cmake,CMakeCache.txt,CTestTestfile.cmake,libtool,config.status,confdefs.h,autom4te,conftest,confstat,Makefile.am,*.gcode,.ninja_deps,.ninja_log,build.ninja,*.csproj,*.m4,*.rej,*.gmo,*.pc,*.omf,*.aux,*.tmp,*.po,*.vm*,*.nvram,*.rcore,*.swp,*.swap,lzo,litmain.sh,*.orig,.histfile.*,.xsession-errors*,*.map,*.so,*.a,*.db,*.qrc,*.ini,*.init,*.img,*.vdi,*.vbox*,vbox.log,*.qcow2,*.vmdk,*.vhd,*.vhdx,*.sql,*.sql.gz,*.ytdl,*.class,*.pyc,*.pyo,*.elc,*.qmlc,*.jsc,*.fastq,*.fq,*.gb,*.fasta,*.fna,*.gbff,*.faa,po,CVS,.svn,.git,_darcs,.bzr,.hg,CMakeFiles,CMakeTmp,CMakeTmpQmake,.moc,.obj,.pch,.uic,.npm,.yarn,.yarn-cache,__pycache__,node_modules,node_packages,nbproject,core-dumps,lost+found,dist,target,build,.terraform,.terragrunt,.gradle,.m2,logs,.lsp,output,out,*.crdownload,*.part,._*,*.vim*tmp,_lock,_cacache,.cache,.cpcache,.deps,.pioenvs,.piolibdeps,.esphome,stable-diffusion-ui,ovos-buildroot,datomic.data,datomic.data*,redis,redis-dev,.Trash,venv
          exclude filters version=8
        '';
      };
  };
}
