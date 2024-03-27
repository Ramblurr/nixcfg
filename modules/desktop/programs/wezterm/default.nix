{ options, config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.programs.wezterm;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in {
  options.modules.desktop.programs.wezterm = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    home-manager.users."${username}" = { ... }@hm: {
      programs.wezterm = {
        enable = true;
        enableZshIntegration = true;
        enableBashIntegration = true;
        extraConfig = ''
          local wezterm = require 'wezterm';

          wezterm.add_to_config_reload_watch_list("${hm.config.xdg.configHome}/wezterm")

          local config = {
            default_prog = { "zsh" },
            enable_tab_bar = false,
            use_fancy_tab_bar = false,
            front_end = "WebGpu",
            initial_rows = 24,
            initial_cols = 120,
            font_size = 13,
            window_background_opacity = 1.0,
            enable_csi_u_key_encoding = true,
            default_cursor_style = 'BlinkingBar',
            color_scheme = 'GruvboxDark',
            enable_wayland = true,
            window_decorations = "RESIZE",
            window_close_confirmation = "NeverPrompt",
            font = wezterm.font_with_fallback({
              {family="${config.modules.desktop.fonts.monospace.family}", weight="Regular"},
              {family="Font Awesome", weight="Regular"},
            })
          }
          return config
        '';
      };
    };
  };
}
