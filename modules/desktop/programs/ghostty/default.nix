{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.ghostty;
  inherit (config.modules.users.primaryUser) username;
  termFont = config.modules.desktop.fonts.terminal;
in
{
  options.modules.desktop.programs.ghostty = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" =
      { pkgs, ... }:
      {
        home.packages = [ pkgs.ghostty ];
        programs.ghostty = {
          enable = true;
          settings = {
            font-family = termFont.name;
            font-size = termFont.size;
            theme = "Gruvbox Dark Hard";
            background-opacity = 1.0;
          };
        };
      };
  };
}
