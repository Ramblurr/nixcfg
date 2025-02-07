{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.desktop.hyprland3;
  nerdfonts = with pkgs; [
    nerd-fonts.iosevka
    nerd-fonts.fira-code
    nerd-fonts.mononoki
    nerd-fonts.jetbrains-mono
    nerd-fonts.symbols-only
  ];

  theme = {
    name = "adw-gtk3-dark";
    package = pkgs.adw-gtk3;
  };
  cursorTheme = {
    name = "Adwaita";
    size = 24;
    package = pkgs.adwaita-icon-theme;
  };
  iconTheme = {
    name = "Adwaita";
    package = pkgs.adwaita-icon-theme;
  };
in

{
  config = lib.mkIf cfg.enable {
    fonts = {
      packages =
        with pkgs;
        [
          theme.package
          cursorTheme.package
          iconTheme.package
          #cantarell-fonts
          font-awesome
          #adwaita-icon-theme
          #papirus-icon-theme
          iosevka-comfy.comfy-fixed
          noto-fonts-emoji
          noto-fonts
          font-awesome
          liberation_ttf # free corefonts-metric-compatible replacement
          ttf_bitstream_vera
          gelasio # metric-compatible with Georgia
          powerline-symbols
        ]
        ++ nerdfonts;

      fontDir.enable = true;

      fontconfig = {
        defaultFonts = {
          serif = [ "Noto Serif" ];
          sansSerif = [ "Noto Sans" ];
          monospace = [ "Iosevka Comfy Fixed" ];
          emoji = [ "Noto Color Emoji" ];
        };
      };
    };

    myhm =
      { ... }@hm:
      {
        home = {
          sessionVariables = {
            XCURSOR_THEME = cursorTheme.name;
            XCURSOR_SIZE = "${toString cursorTheme.size}";
          };
          pointerCursor = cursorTheme // {
            gtk.enable = true;
          };
          file = {
            ".local/share/themes/${theme.name}" = {
              source = "${theme.package}/share/themes/${theme.name}";
            };
          };
        };

        gtk = {
          enable = true;
          theme.name = theme.name;
          inherit cursorTheme iconTheme;
        };

        qt = {
          enable = true;
          platformTheme.name = "kde";
        };
      };
  };
}
