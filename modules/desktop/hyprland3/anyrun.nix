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

        imports = [ inputs.anyrun.homeManagerModules.default ];
        programs.anyrun = {
          enable = true;
          config = {
            plugins = [
              # An array of all the plugins you want, which either can be paths to the .so files, or their packages
              inputs.anyrun.packages.${pkgs.system}.applications
              inputs.anyrun.packages.${pkgs.system}.rink
              inputs.anyrun.packages.${pkgs.system}.translate
              inputs.anyrun.packages.${pkgs.system}.shell
              inputs.anyrun.packages.${pkgs.system}.symbols
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
              border-radius: 16px;
              border: 1px solid transparent;
            }

            #match:selected {
              border: 1px solid @color-bg-selected;
            }

            box#main {
              background: @color-bg;
              /*border: 1px solid @color-border;*/
              border:0px;
              border-radius: 16px;
              padding: 8px
            }


            #entry,
            #plugin:hover {
              border: 3px solid #b8bb26;
              border-radius: 16px;
            }

          '';
          # end extra css
          extraConfigFiles = {
            "applications.ron".text = ''
              Config(
                // Also show the Desktop Actions defined in the desktop files, e.g. "New Window" from LibreWolf
                desktop_actions: true,
                max_entries: 10,
                // The terminal used for running terminal based desktop entries, if left as `None` a static list of terminals is used
                // to determine what terminal to use.
                terminal: Some("foot"),
              )
            '';
            "symbols.ron".text = ''
              Config(
                // The prefix that the search needs to begin with to yield symbol results
                prefix: ":sy",

                // Custom user defined symbols to be included along the unicode symbols
                symbols: {
                  // "name": "text to be copied"
                  "shrug": "¯\\_(ツ)_/¯",
                },

                // The number of entries to be displayed
                max_entries: 5,
              )
            '';

            "translate.ron".text = ''
              Config(
                prefix: ":tr",
                language_delimiter: ">",
                max_entries: 3,
              )
            '';
            "dictionary.ron".text = ''
              Config(
                prefix: ":d",
                max_entries: 5,
              )
            '';

            "shell.ron".text = ''
              Config(
                prefix: ":sh",
                shell: "zsh",
                max_entries: 5,
              )
            '';
            #"nixos-options.ron".text =
            #  let
            #    nixos-options = osConfig.system.build.manual.optionsJSON + "/share/doc/nixos/options.json";
            #    hm-options = inputs.home-manager.packages.docs-json + "/share/doc/home-manager/options.json";
            #    options = builtins.toJSON {
            #      ":nix" = [ nixos-options ];
            #      ":hm" = [ hm-options ];
            #      ":nall" = [
            #        nixos-options
            #        hm-options
            #      ];
            #    };
            #  in
            #  ''
            #    Config(
            #      options: ${options},
            #      min_score: 5,
            #      max_entries: Some(3),
            #    )
            #  '';
          };
        };
      };
  };
}
