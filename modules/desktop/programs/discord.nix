{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.discord;
  inherit (config.modules.users.primaryUser) username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.discord = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          discord
          betterdiscordctl
        ];
        home.persistence."/persist" = mkIf withImpermanence {
          directories = [
            ".config/BetterDiscord"
            ".config/discord"
          ];
        };
      };
  };
}
