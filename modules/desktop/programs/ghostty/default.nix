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
          settings =
            (
              if termFont.name == "Berkeley Mono" then
                {
                  font-family = "Berkeley Mono";
                  font-style = "ExtraCondensed";
                  font-size = 12.0;
                  font-feature = [
                    "+calt"
                    "+ss02"
                  ];
                }
              else
                {
                  font-family = ''"${termFont.name}"'';
                  font-size = termFont.size;
                  font-feature = [
                    "calt"
                    "dlig"
                    "zero"
                  ];
                  font-style = "auto";
                  font-thicken = true;
                  adjust-cell-height = "10%";
                }
            )
            // {
              theme = "Gruvbox Dark Hard";
              background-opacity = 1.0;
              window-decoration = false;
              app-notifications = "no-clipboard-copy";
            };
        };
      };
  };
}
