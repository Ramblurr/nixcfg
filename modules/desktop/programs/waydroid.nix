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
  cfg = config.modules.desktop.programs.waydroid;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.waydroid = {
    enable = lib.mkEnableOption "";
    autoStart.enable = mkEnableOption "Waydroid auto start";
  };

  config = mkIf cfg.enable {

    virtualisation.waydroid.enable = true;
    environment.persistence."/persist" = mkIf withImpermanence {
      directories = [
        "/var/lib/waydroid"
      ];
      users.${username} = {
        directories = [ ".local/share/waydroid" ];
      };
    };

    systemd.services."waydroid-container".wantedBy = mkForce (
      optional cfg.autoStart.enable "multi-user.target"
    );

    home-manager.users."${username}" =
      { pkgs, config, ... }@hm:
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
