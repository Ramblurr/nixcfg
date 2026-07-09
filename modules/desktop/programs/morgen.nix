{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.morgen;
in
{
  options.modules.desktop.programs.morgen = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    myhm = {
      home.packages = with pkgs; [
        morgen
      ];
    };
  };
}
