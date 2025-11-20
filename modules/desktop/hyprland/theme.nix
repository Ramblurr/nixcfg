{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.desktop.hyprland;
  nerdfonts = with pkgs; [
    nerd-fonts.iosevka
    nerd-fonts.fira-code
    nerd-fonts.mononoki
    nerd-fonts.jetbrains-mono
    nerd-fonts.symbols-only
  ];

  #font.mono = "Iosevka Comfy Fixed";
  #font.mono = "Berkeley Mono Trial";
  font.mono = "Iosevka";
  font.term = "Iosevka Nerd Font Mono";
  font.serif = "Noto Serif";
  font.sans = "Nokia Sans Wide";
  font.emoji = "Noto Color Emoji";

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
  config = lib.mkIf cfg.enable { };
}
