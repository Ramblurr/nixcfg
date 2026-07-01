{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.musescore;
  inherit (config.modules.users.primaryUser) username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.musescore = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {

    home-manager.users."${username}" =
      { pkgs, ... }:
      {
        home.packages = [
          pkgs.musescore
          pkgs.muse-sounds-manager
        ];
      };
  };
}
