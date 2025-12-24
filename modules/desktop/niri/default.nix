{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.niri;
  inherit (config.modules.users.primaryUser) username;
  withImpermanence = config.modules.impermanence.enable;

  niri-emacs = pkgs.writeScriptBin "niri-emacs" ''
    if [[ $(niri msg --json focused-window | jq -r .app_id) == "emacs" ]]; then
        command="(my/emacs-niri-integration \"$@\")"
        echo "emacs -> $command" >> ~/emacs-wm.log
        emacsclient -e "$command" >> ~/emacs-wm.log
    else
        echo "hypr -> $@" >> ~/emacs-wm.log
        niri msg action "$@"
    fi
  '';
  extraIncludesText = lib.concatMapStringsSep "\n" (name: ''include "${name}"'') (
    lib.attrNames cfg.extraIncludes ++ lib.optional config.modules.editors.emacs.enable "emacs.kdl"
  );
  inherit (inputs.niri.packages.${pkgs.system}) niri;
in
{
  options.modules.desktop.niri = {
    enable = lib.mkEnableOption "Enable Niri";
    extraIncludes = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      description = "Additional KDL files to include. Keys are filenames, values are file contents.";
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.enable && !config.modules.desktop.kde.enable;
        message = "My niri config is mutually exclusive with KDE Plasma";
      }
    ];
    programs.niri.enable = true;
    programs.niri.package = niri;
    environment.persistence."/persist" = lib.mkIf withImpermanence {
      users.${username} = {
        directories = [
          ".config/niri"
        ];
      };
    };
    environment.etc."nvidia/nvidia-application-profiles-rc.d/50-limit-free-buffer-pool-in-wayland-compositors.json".text =
      # ref: https://github.com/YaLTeR/niri/wiki/Nvidia
      ''
        {"rules": [{"pattern": {"feature": "procname", "matches": "niri"}, "profile": "Limit Free Buffer Pool On Wayland Compositors"}],
         "profiles": [{"name": "Limit Free Buffer Pool On Wayland Compositors", "settings": [{"key": "GLVidHeapReuseRatio", "value": 0}]}]}
      '';
    myhm = {
      home.packages = with pkgs; [
        # Niri v25.08 will create X11 sockets on disk, export $DISPLAY, and spawn `xwayland-satellite` on-demand when an X11 client connects
        xwayland-satellite
        fuzzel
      ];
      xdg.configFile = {
        "niri/config.kdl" = {
          source = pkgs.replaceVars ./config.kdl {
            extraIncludes = extraIncludesText;
          };
        };
      }
      // lib.mapAttrs' (name: value: lib.nameValuePair "niri/${name}" value) (
        lib.genAttrs [ "outputs.kdl" "rules.kdl" "binds.kdl" ] (f: {
          source = ./${f};
        })
      )
      // lib.mapAttrs' (name: text: lib.nameValuePair "niri/${name}" { inherit text; }) cfg.extraIncludes
      // lib.optionalAttrs config.modules.editors.emacs.enable {
        "niri/emacs.kdl" = {
          source = pkgs.replaceVars ./emacs.kdl {
            emacsWM = "${niri-emacs}/bin/niri-emacs";
          };
        };
      };

      services.swayidle =
        let
          idleTimeout = 500;
        in
        {
          enable = true;
          timeouts = [
            {
              timeout = idleTimeout;
              # mark desktop as idle
              command = ''${pkgs.coreutils}/bin/touch "$XDG_RUNTIME_DIR/desktop-idle"'';
              # clear idle flag on user activity
              resumeCommand = ''${pkgs.coreutils}/bin/rm -f "$XDG_RUNTIME_DIR/desktop-idle"'';
            }
            {
              timeout = idleTimeout;
              command = "${niri}/bin/niri msg action power-off-monitors";
            }
          ];
        };

    };
  };
}
