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
  cfg = config.modules.desktop.programs.signal;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
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
      { pkgs, config, ... }@hm:
      {
        home.packages = [ pkgs.signal-desktop ];
        home.file.".local/share/applications/signal-desktop.desktop" = {
          text = ''
            [Desktop Entry]
            Name=Signal Desktop
            Exec=/nix/store/pkdmssw4rvnbd55i1x2ha4bx6liv1fdz-signal-desktop-7.14.0/bin/signal-desktop --ozone-platform-hint=auto --no-sandbox %U
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
