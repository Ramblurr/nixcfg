{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.impermanence;
  username = config.modules.users.primaryUser.username;
in
{
  environment.persistence."/persist" = {
    users.${username} = {
      directories = [
        "docs"
        "downloads"
        "src"
        "sync"
        "vendor"
        "work"
        ".cache/audacity"
        ".cache/gimp"
        ".cache/inkscape"
        ".cache/virt-manager"
        ".config/audacity"
        ".config/easyeffects"
        ".config/sops"
        ".config/GIMP"
        ".config/gnupg"
        ".config/inkscape"
        ".config/Morgen"
        ".config/MuseScore"
        ".config/OpenSCAD"
        ".config/PrusaSlicer"
        ".config/PrusaSlicer-alpha"
        ".config/qobuz-dl"
        ".config/rclone"
        ".local/bin"
        ".local/share/audacity"
        ".local/share/fonts"
        ".local/share/krita"
        ".local/share/MuseScore"
        ".local/share/OpenSCAD"
        ".local/state/audacity"
        ".local/state/MuseScore"
        ".local/state/muse-sounds-manager"
      ];
    };
  };
}
