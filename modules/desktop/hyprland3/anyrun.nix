{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.desktop.hyprland3;
in
{
  config = lib.mkIf cfg.enable {

    myhm =
      { ... }@hm:
      {

        programs.anyrun = {
          enable = lib.mkForce true;
          config = {
            plugins = [
              "${pkgs.anyrun}/lib/libapplications.so"
              "${pkgs.anyrun}/lib/libsymbols.so"
              "${pkgs.anyrun}/lib/libshell.so"
              "${pkgs.anyrun}/lib/librink.so"
            ];

            # x and y are the position of the main box
            x.fraction = 0.5;
            y.absolute = 16;
            width.fraction = 0.3;
            height.absolute = 0;
            hideIcons = false;
            ignoreExclusiveZones = false;
            layer = "overlay";
            hidePluginInfo = true;
            closeOnClick = true;
            showResultsImmediately = false;
            maxEntries = 3;

          };
          extraCss = ''
            @define-color color-transparent transparent;
            @define-color color-bg-selected #fabd2f;
            @define-color color-bg #282828;
            @define-color color-border #1d2021;

            * {
              transition: 200ms ease;
              font-size: 1.3rem;
            }

            #window {
              background: transparent;
            }

            #match,
            #entry,
            #plugin,
            #main {
              background: transparent;
            }


            #match {
              padding: 3px;
              border-radius: 8px;
              border: 1px solid transparent;
            }

            #match:selected {
              border: 1px solid @color-bg-selected;
            }

            box#main {
              background: @color-bg;
              /*border: 1px solid @color-border;*/
              border:0px;
              border-radius: 8px;
              padding: 8px
            }


            #entry,
            #plugin:hover {
              border: 2px solid #b8bb26;
              border-radius: 8px;
            }

          '';
        };
      };
  };
}
