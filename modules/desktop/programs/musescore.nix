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
  cfg = config.modules.desktop.programs.musescore;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
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
      { pkgs, config, ... }@hm:
      {
        home.packages = [
          pkgs.musescore
          # https://nixpk.gs/pr-tracker.html?pr=341856
          #pkgs.muse-sounds-manager
        ];
      };
  };
}
