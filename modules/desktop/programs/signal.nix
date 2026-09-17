{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.signal;
  inherit (config.modules.users.primaryUser) username;
in
{
  options.modules.desktop.programs.signal = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {

    home-manager.users."${username}" =
      { config, pkgs, ... }:
      {
        home.packages = [ pkgs.signal-desktop ];
        home.file.".local/share/applications/signal-desktop.desktop" = {
          text = ''
            [Desktop Entry]
            Name=Signal Desktop (Personal)
            Exec=${pkgs.signal-desktop}/bin/signal-desktop --ozone-platform-hint=auto --no-sandbox %U
            Terminal=false
            Type=Application
            Icon=signal-desktop
            StartupWMClass=signal
            Comment=Private messaging from your desktop
            MimeType=x-scheme-handler/sgnl;x-scheme-handler/signalcaptcha;
            Categories=Network;InstantMessaging;Chat;
          '';
        };
        home.file.".local/share/applications/signal-desktop-work.desktop" = {
          text = ''
            [Desktop Entry]
            Name=Signal Desktop (Work)
            Exec=${pkgs.signal-desktop}/bin/signal-desktop "--user-data-dir=${config.xdg.configHome}/Signal-Work" --ozone-platform-hint=auto --no-sandbox %U
            Terminal=false
            Type=Application
            Icon=signal-desktop
            StartupWMClass=signal
            Comment=Work messaging with a separate Signal profile
            Categories=Network;InstantMessaging;Chat;
          '';
        };
      };
  };
}
