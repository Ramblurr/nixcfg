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
  cfg = config.modules.desktop.programs.kicad;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.kicad = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" =
      { pkgs, config, ... }@hm:
      {
        home.packages = with pkgs; [ kicad ];
        home.persistence."/persist${homeDirectory}" = mkIf withImpermanence {
          directories = [
            ".config/kicad"
            ".config/kicad5"
          ];
        };
      };
  };
}
