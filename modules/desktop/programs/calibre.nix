{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.calibre;
  inherit (config.modules.users.primaryUser) username;
  inherit (config.modules.users.primaryUser) homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.calibre = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [ calibre ];
        home.persistence."/persist${homeDirectory}" = mkIf withImpermanence {
          directories = [ ".config/calibre" ];
        };
      };
  };
}
