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
  cfg = config.modules.desktop.kde;
  username = config.modules.users.primaryUser.username;
  withImpermanence = config.modules.impermanence.enable;

  nerdfonts = (
    pkgs.nerdfonts.override {
      fonts = [
        "Iosevka"
        "FiraCode"
        "Mononoki"
        "JetBrainsMono"
        "NerdFontsSymbolsOnly"
      ];
    }
  );

  font = {
    name = "Noto Nerd Font";
    package = nerdfonts;
  };
in
{
  options.modules.desktop.kde = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.enable && config.modules.desktop.hyprland2 != true;
        message = "My KDE config is mutually exclusive with hyprland";
      }
    ];
    services.displayManager.enable = true;
    services.desktopManager.plasma6.enable = true;
    services.displayManager.sddm.wayland.enable = true;
    services.displayManager.sddm.enable = true;
    programs.dconf.enable = true;

    environment.systemPackages = [
      pkgs.kdePackages.krohnkite
      pkgs.kwin6-bismuth-decoration
      pkgs.klassy
    ];

    environment.plasma6.excludePackages = [ pkgs.kdePackages.khelpcenter ];

    environment.persistence."/persist".directories = lib.mkIf withImpermanence [
      "/var/lib/NetworkManager"
      "/etc/NetworkManager/system-connections"
    ];

    fonts = {

      packages = with pkgs; [
        cantarell-fonts
        cabin
        font-awesome
        font.package
        iosevka-comfy.comfy-fixed
        noto-fonts-emoji
        noto-fonts
        font-awesome
        liberation_ttf # free corefonts-metric-compatible replacement
        ttf_bitstream_vera
        gelasio # metric-compatible with Georgia
        powerline-symbols
      ];

      fontDir.enable = true;

      fontconfig = {
        defaultFonts = {
          serif = [ "Noto Serif" ];
          sansSerif = [ "Cabin" ];
          monospace = [ "Iosevka Comfy Fixed" ];
          emoji = [ "Noto Color Emoji" ];
        };
      };
    };

    myhm =
      { ... }@hm:
      {
        # Make certain user services happy
        # https://github.com/nix-community/home-manager/issues/2064
        systemd.user.targets.tray = {
          Unit = {
            Description = "Home Manager System Tray";
            Requires = [ "graphical-session-pre.target" ];
          };
        };
      }; # end home manager
  };
}
