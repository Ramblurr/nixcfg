{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.shell.htop;
  inherit (config.modules.users.primaryUser) username;
in
{
  options.modules.shell.htop = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" = {
      xdg.configFile."btop/themes/gruvbox_dark_v2.theme".source = ./configs/btop/gruvbox_dark_v2.theme;
      programs.btop = {
        enable = true;
        settings = {
          color_theme = "gruvbox_dark_v2";
          theme_background = true;
          rounded_corners = true;
          vim_keys = true;
        };
      };
      programs.htop = {
        enable = true;
        package = pkgs.htop-vim;
        settings = {
          hide_kernel_threads = true;
          hide_userland_threads = true;
          highlight_base_name = true;
          highlight_changes = true;
          show_cpu_frequency = true;
          show_cpu_temperature = true;
          show_program_path = false;
          tree_view = true;
        };
      };
    };
  };
}
