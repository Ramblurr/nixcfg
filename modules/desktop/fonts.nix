{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.desktop.fonts;
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
  iosevka-etoile = pkgs.iosevka-bin.override { variant = "Etoile"; };
  iosevka-aile = pkgs.iosevka-bin.override { variant = "Aile"; };

  # iosevka (ss15 = IBM Plex Mono Style)
  #iosevka-ss15 = pkgs.iosevka-bin.override { variant = "SS15"; };
  #default-mono.pkg = iosevka-ss15;
  #default-mono.name = "Iosevka SS15";
  #default-mono.term = "Iosevka Term SS15";

  # monaspace
  # note(2026-04): i evaluated monaspace as a mono font, in gernaly i like the glyphs and ligatures
  #                however, it is much much to wide, needlessly increasing col width. there's an open issue about this maybe someday it will be fixed0
  #                ref: https://github.com/githubnext/monaspace/issues/22
  #default-mono.pkg = pkgs.monaspace;
  #default-mono.name = "Monaspace Neon";
  #default-mono.term = "Monaspace Neon";

  # ramsevka, my custom iosevka build
  ramsevka = inputs.ramsevka.packages.${pkgs.stdenv.hostPlatform.system};
  default-mono.pkg = ramsevka.ramsevka-full;
  default-mono.name = "Ramsevka Mono";
  default-mono.term = "Ramsevka Term";
in
{

  options.modules.desktop.fonts = {
    enable = lib.mkEnableOption "Enable fonts";
    packages = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ iosevka-etoile ];
    };
    serif = {
      name = lib.mkOption {
        type = lib.types.str;
        #default = "Noto Serif";
        default = "Iosevka Etoile";
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
        #default = "Noto Sans";
        default = "Iosevka Aile";
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
        default = default-mono.name;
      };
      package = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        default = default-mono.pkg;
      };
      size = lib.mkOption {
        type = lib.types.float;
        default = 12.0;
      };
    };
    terminal = {
      name = lib.mkOption {
        type = lib.types.str;
        default = default-mono.term;
      };
      package = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        default = default-mono.pkg;
      };
      size = lib.mkOption {
        type = lib.types.float;
        default = 12.0;
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
        pkgs.unscii
        iosevka-etoile
        iosevka-aile
        pkgs.iosevka-bin
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
    myhm = _: {
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
          inherit (cfg.sans) package;
          inherit (cfg.sans) name;
          inherit (cfg.sans) size;
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
