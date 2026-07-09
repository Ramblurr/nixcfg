{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.nheko;
  inherit (config.modules.users.primaryUser) username;
in
{
  options.modules.desktop.programs.nheko = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [ nheko ];
      };
  };
}
