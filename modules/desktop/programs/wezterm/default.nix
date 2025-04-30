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
  cfg = config.modules.desktop.programs.wezterm;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
  termFont = config.modules.desktop.fonts.terminal;
in
{
  options.modules.desktop.programs.wezterm = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" =
      { pkgs, config, ... }@hm:
      {
        home.packages = [ pkgs.wezterm ];
        programs.wezterm = {
          enable = true;
          extraConfig = ''
            local wezterm = require 'wezterm'
            local config = wezterm.config_builder()
            config.mux_enable_ssh_agent = false

            --config.color_scheme = 'Gruvbox dark, hard (base16)'
            --config.color_scheme = 'GruvboxDarkHard'
            config.color_scheme = 'GruvboxDark'
            config.font = wezterm.font_with_fallback {
               "${termFont.name}",
               --"Berkeley Mono Trial",
               -- "Iosevka Nerd Font Mono",
               --"Iosevka Term",
               --"Iosevka Comfy Fixed",
               --"Symbols Nerd Font Mono",
               "Noto Color Emoji"
             }
            config.font_size = ${toString termFont.size};
            config.freetype_load_flags = 'NO_HINTING'
            --config.freetype_load_target = 'Normal'
            config.freetype_load_target = 'Light'
            --config.freetype_load_target = 'HorizontalLcd'
            --config.freetype_load_target = 'VerticalLcd'
            --config.front_end = "OpenGL"
            config.front_end = "WebGpu"
            --config.line_height = 0.9
            config.hide_tab_bar_if_only_one_tab = true

            config.enable_scroll_bar = true
            config.window_padding = {
              left = 0,
              right = 5,
              top = 0,
              bottom = 0,
            }

            return config

          '';
        };
      };
  };
}
