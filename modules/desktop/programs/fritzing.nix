{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.fritzing;
  inherit (config.modules.users.primaryUser) username;
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
        home.persistence."/persist" = mkIf withImpermanence {
          directories = [ ".config/Fritzing" ];
        };
      };
  };
}
