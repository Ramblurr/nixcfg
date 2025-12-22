{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.nheko;
  inherit (config.modules.users.primaryUser) username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.nheko = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    environment.persistence."/persist" = mkIf withImpermanence {
      users.${username} = {
        directories = [
          ".config/nheko"
          ".cache/nheko"
          ".local/share/nheko"
        ];
      };
    };
    home-manager.users."${username}" =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [ nheko ];
      };
  };
}
