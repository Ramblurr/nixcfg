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
  cfg = config.modules.desktop.programs.ghostty;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
  termFont = config.modules.desktop.fonts.terminal;
in
{
  options.modules.desktop.programs.ghostty = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" =
      { pkgs, config, ... }@hm:
      {
        home.packages = [ pkgs.ghostty ];
        programs.ghostty = {
          enable = true;
          settings = {
            font-family = termFont.name;
            font-size = termFont.size;
            theme = "Gruvbox Dark Hard";
            background-opacity = 0.8;
          };
        };
      };
  };
}
