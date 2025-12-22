{
  config,
  lib,
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
        settings = {
          hide_kernel_threads = 1;
          hide_userland_threads = 1;
        };
      };
    };
  };
}
