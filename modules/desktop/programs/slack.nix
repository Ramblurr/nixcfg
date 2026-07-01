{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.slack;
  inherit (config.modules.users.primaryUser) username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.slack = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {

    home-manager.users."${username}" =
      { pkgs, ... }:
      {
        home.packages = [ pkgs.slack ];
      };
  };
}
