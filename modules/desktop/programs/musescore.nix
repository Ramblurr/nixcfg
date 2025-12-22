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

    environment.persistence."/persist" = mkIf withImpermanence {
      users.${username} = {
        directories = [
          ".config/MuseScore"
          ".local/share/MuseScore"
          ".local/state/MuseScore"
          ".local/state/muse-sounds-manager"
          ".local/share/MuseSampler"
          ".muse-hub"
        ];
      };
    };
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
