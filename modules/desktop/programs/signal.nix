{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.signal;
  inherit (config.modules.users.primaryUser) username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.signal = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {

    environment.persistence."/persist" = mkIf withImpermanence {
      users.${username} = {
        directories = [ ".config/Signal" ];
      };
    };
    home-manager.users."${username}" =
      { pkgs, ... }:
      {
        home.packages = [ pkgs.signal-desktop ];
        home.file.".local/share/applications/signal-desktop.desktop" = {
          text = ''
            [Desktop Entry]
            Name=Signal Desktop (nix)
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
      };
  };
}
