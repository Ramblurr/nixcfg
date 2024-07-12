{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  mainSansFont = "Cabin";
in
{
  home-manager.sharedModules = [ inputs.plasma-manager.homeManagerModules.plasma-manager ];
  myhm =
    { ... }@hm:
    {
      # Starship prompt color - something very different to distinguish it from my main workstation
      programs.starship.settings = pkgs.lib.importTOML ./gruvbox-rainbow.toml;

      # Make certain user services happy
      # https://github.com/nix-community/home-manager/issues/2064
      systemd.user.targets.tray = {
        Unit = {
          Description = "Home Manager System Tray";
          Requires = [ "graphical-session-pre.target" ];
        };
      };

      # Apps, apps, apps
      home.packages = with pkgs; [
        musescore
        audacity
        pavucontrol
        brightnessctl
        meld
        gimp
        pdfarranger
        xournal
        qpwgraph # pipewire wiring gui tool
        easyeffects # pipewire eq
      ];

      # Plasma config
      programs.plasma = {
        enable = true;
        fonts = {
          general = {
            family = mainSansFont;
            pointSize = 12;
          };
          fixedWidth = {
            family = "Iosevka Comfy Fixed";
            pointSize = 12;
          };
          small = {
            family = mainSansFont;
            pointSize = 10;

          };
          menu = {
            family = mainSansFont;
            pointSize = 12;

          };
          windowTitle = {
            family = mainSansFont;
            pointSize = 10;
          };
        }; # end fonts

        hotkeys.commands."launch-konsole" = {
          name = "Launch Konsole";
          key = "Meta+Return";
          command = "konsole";
        };
        workspace = {
          lookAndFeel = "org.kde.breezedark.desktop";
        }; # end workspace

        panels = [
          # Windows-like panel at the bottom
          {
            location = "bottom";
            widgets = [
              # We can configure the widgets by adding the name and config
              # attributes. For example to add the the kickoff widget and set the
              # icon to "nix-snowflake-white" use the below configuration. This will
              # add the "icon" key to the "General" group for the widget in
              # ~/.config/plasma-org.kde.plasma.desktop-appletsrc.
              {
                name = "org.kde.plasma.kickoff";
                config = {
                  General.icon = "nix-snowflake-white";
                };
              }
              # Adding configuration to the widgets can also for example be used to
              # pin apps to the task-manager, which this example illustrates by
              # pinning dolphin and konsole to the task-manager by default.
              {
                name = "org.kde.plasma.icontasks";
                config = {
                  General.launchers = [
                    "applications:org.kde.dolphin.desktop"
                    "applications:org.kde.konsole.desktop"
                    "applications:signal-desktop.desktop"
                    "applications:firefox-personal.desktop"
                  ];
                };
              }
              # If no configuration is needed, specifying only the name of the
              # widget will add them with the default configuration.
              "org.kde.plasma.marginsseparator"
              # If you need configuration for your widget, instead of specifying the
              # the keys and values directly using the config attribute as shown
              # above, plasma-manager also provides some higher-level interfaces for
              # configuring the widgets. See modules/widgets for supported widgets
              # and options for these widgets. The widgets below shows two examples
              # of usage, one where we add a digital clock, setting 12h time and
              # first day of the week to Sunday and another adding a systray with
              # some modifications in which entries to show.
              {
                digitalClock = {
                  calendar.firstDayOfWeek = "sunday";
                  time.format = "24h";
                };
              }
              {
                systemTray.items = {
                  shown = [
                    "org.kde.plasma.volume"
                    "org.kde.plasma.battery"
                  ];
                  hidden = [
                    "org.kde.plasma.networkmanagement"
                    "org.kde.plasma.bluetooth"
                  ];
                };
              }
            ];
            hiding = "autohide";
          }
          # Global menu at the top
          {
            location = "top";
            height = 26;
            widgets = [ "org.kde.plasma.appmenu" ];
          }
        ]; # end panels

        configFile = {
          "baloofilerc"."General"."exclude filters".value = "*~,*.part,*.o,*.la,*.lo,*.loT,*.moc,moc_*.cpp,qrc_*.cpp,ui_*.h,cmake_install.cmake,CMakeCache.txt,CTestTestfile.cmake,libtool,config.status,confdefs.h,autom4te,conftest,confstat,Makefile.am,*.gcode,.ninja_deps,.ninja_log,build.ninja,*.csproj,*.m4,*.rej,*.gmo,*.pc,*.omf,*.aux,*.tmp,*.po,*.vm*,*.nvram,*.rcore,*.swp,*.swap,lzo,litmain.sh,*.orig,.histfile.*,.xsession-errors*,*.map,*.so,*.a,*.db,*.qrc,*.ini,*.init,*.img,*.vdi,*.vbox*,vbox.log,*.qcow2,*.vmdk,*.vhd,*.vhdx,*.sql,*.sql.gz,*.ytdl,*.class,*.pyc,*.pyo,*.elc,*.qmlc,*.jsc,*.fastq,*.fq,*.gb,*.fasta,*.fna,*.gbff,*.faa,po,CVS,.svn,.git,_darcs,.bzr,.hg,CMakeFiles,CMakeTmp,CMakeTmpQmake,.moc,.obj,.pch,.uic,.npm,.yarn,.yarn-cache,__pycache__,node_modules,node_packages,nbproject,core-dumps,lost+found,.git,.terraform,build,dist,target,.cpcache,.clj-kondo,.lsp,.portal,bin,env";
          "baloofilerc"."General"."exclude foldersx5b$ex5d".value = "$HOME/";
          "baloofilerc"."General"."foldersx5b$ex5d".value = "$HOME/docs,$HOME/sync";
        }; # end configFile
      }; # end plasma

    }; # end home manager
}
