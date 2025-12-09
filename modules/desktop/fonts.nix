{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.desktop.fonts;
  username = config.modules.users.primaryUser.username;
  withImpermanence = config.modules.impermanence.enable;
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
  mkAlias = font: ''
    <alias>
      <family>${font}</family>
      <prefer><family>${cfg.symbols.name}</family></prefer>
    </alias>
  '';

  aliases = builtins.map mkAlias cfg.symbols.fonts;
  aliases' = lib.strings.concatLines aliases;
in
{

  options.modules.desktop.fonts = {
    enable = lib.mkEnableOption "Enable fonts";
    packages = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ ];
    };
    serif = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "Noto Serif";
      };
      package = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        default = pkgs.noto-fonts;
      };
      size = lib.mkOption {
        type = lib.types.float;
        default = 12.0;
      };
    };
    sans = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "Noto Sans";
      };
      package = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        default = pkgs.noto-fonts;
      };
      size = lib.mkOption {
        type = lib.types.float;
        default = 12.0;
      };
    };
    emoji = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "Noto Color Emoji";
      };
      package = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        default = pkgs.noto-fonts-color-emoji;
      };
      size = lib.mkOption {
        type = lib.types.float;
        default = 12.0;
      };
    };
    mono = {
      name = lib.mkOption {
        type = lib.types.str;
      };
      package = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
      };
      size = lib.mkOption {
        type = lib.types.float;
        default = 12.0;
      };
    };
    terminal = {
      name = lib.mkOption {
        type = lib.types.str;
        default = cfg.mono.name;
      };
      package = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        default = cfg.mono.package;
      };
      size = lib.mkOption {
        type = lib.types.float;
        default = 10.0;
      };
    };

    symbols = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "Symbols Nerd Font";
      };
      package = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        default = pkgs.nerd-fonts.symbols-only;
      };
      fonts = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        example = [
          "Fira Code"
          "Fira Code,Fira Code Light"
        ];
        description = ''
          Fonts to enable Nerd Fonts symbols for.
        '';
      };
    };
  };
  config = lib.mkIf cfg.enable {
    fonts = {
      packages = [
        cfg.sans.package
        cfg.serif.package
        cfg.mono.package
        cfg.terminal.package
        cfg.emoji.package
        cfg.symbols.package
        theme.package
        cursorTheme.package
        iconTheme.package
      ]
      ++ cfg.packages;

      fontDir.enable = true;

      fontconfig = {
        defaultFonts = {
          serif = [ cfg.serif.name ];
          sansSerif = [ cfg.sans.name ];
          monospace = [ cfg.mono.name ];
          emoji = [ cfg.emoji.name ];
        };

        localConf = ''
          <?xml version="1.0"?>
          <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
          <fontconfig>
            ${aliases'}
          </fontconfig>
        '';
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
          font = {
            package = cfg.sans.package;
            name = cfg.sans.name;
            size = cfg.sans.size;
          };
        };

        qt = {
          # TODO: renable after  https://github.com/nix-community/home-manager/issues/7728
          enable = false;
          platformTheme.name = "kde";
        };
      };
  };
}
