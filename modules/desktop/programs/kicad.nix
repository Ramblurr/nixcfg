{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.kicad;
  inherit (config.modules.users.primaryUser) username;
  inherit (config.modules.users.primaryUser) homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.kicad = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" =
      { pkgs, ... }:
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
