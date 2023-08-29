{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.fonts;
  username = config.modules.users.primaryUser.username;
in {
  options.modules.desktop.fonts = {
    enable = mkBoolOpt false;
    sans = {
      family = mkStrOpt "Noto Sans";
      package = mkPkgOpt pkgs.noto-fonts;
    };
    serif = {
      family = mkStrOpt "Noto Serif";
      package = mkPkgOpt pkgs.noto-fonts;
    };
    monospace = {
      family = mkStrOpt "Iosevka Comfy Fixed";
      package = mkPkgOpt pkgs.iosevka-comfy.comfy-fixed;
    };
    emoji = {
      family = mkStrOpt "Noto Color Emoji";
      package = mkPkgOpt pkgs.noto-fonts-emoji;
    };
    fallback = {
      family = mkStrOpt "Font Awesome 5 Free";
      package = mkPkgOpt pkgs.font-awesome;
    };
  };
  config = mkIf cfg.enable {
    # TODO(upgrade) fonts.fonts to fonts.packages once everything is on 23.11
    fonts = {
      fonts =
        (map (p: p.package)
          [
            cfg.sans
            cfg.serif
            cfg.monospace
            cfg.fallback
            cfg.emoji
          ])
        ++ (with pkgs; [
          liberation_ttf # free corefonts-metric-compatible replacement
          ttf_bitstream_vera
          gelasio # metric-compatible with Georgia
          powerline-symbols
          (nerdfonts.override {fonts = ["Iosevka" "FiraCode" "Mononoki" "JetBrainsMono" "NerdFontsSymbolsOnly"];})
        ]);

      fontDir.enable = true;

      fontconfig = {
        defaultFonts = {
          serif = [cfg.serif.family];
          sansSerif = [cfg.sans.family];
          monospace = [cfg.sans.family];
          emoji = [cfg.emoji.family];
        };
      };
    };
  };
}
