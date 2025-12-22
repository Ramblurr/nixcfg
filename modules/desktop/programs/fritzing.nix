{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.fritzing;
  inherit (config.modules.users.primaryUser) username;
  inherit (config.modules.users.primaryUser) homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.fritzing = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" =
      { pkgs, ... }:
      {
        home.packages = [ pkgs.fritzing ];
        home.persistence."/persist${homeDirectory}" = mkIf withImpermanence {
          directories = [ ".config/Fritzing" ];
        };
      };
  };
}
