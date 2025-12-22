{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.junction;
  inherit (config.modules.users.primaryUser) username;
in
{
  options.modules.desktop.programs.junction = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" =
      { pkgs, ... }:
      {
        home.packages = [ pkgs.junction ];
      };
  };
}
