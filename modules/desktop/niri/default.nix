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
  cfg = config.modules.desktop.niri;
  username = config.modules.users.primaryUser.username;
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
in
{
  options.modules.desktop.niri = {
    enable = lib.mkEnableOption "Enable Niri";
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.enable && config.modules.desktop.kde != true;
        message = "My niri config is mutually exclusive with KDE Plasma";
      }
    ];
    programs.niri.enable = true;
    environment.persistence."/persist" = lib.mkIf withImpermanence {
      users.${username} = {
        directories = [
          ".config/niri"
        ];
      };
    };
    myhm = {
      home.packages = with pkgs; [
        # Niri v25.08 will create X11 sockets on disk, export $DISPLAY, and spawn `xwayland-satellite` on-demand when an X11 client connects
        xwayland-satellite
        fuzzel
      ];

      xdg.configFile."niri/config-nix.kdl" = {
        source = pkgs.replaceVars ./config.kdl {
          emacsWM = "${niri-emacs}/bin/niri-emacs";
          DEFAULT_AUDIO_SOURCE = null;
          DEFAULT_AUDIO_SINK = null;
        };
      };
    };
  };
}
