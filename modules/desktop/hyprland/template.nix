{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.desktop.hyprland;
in

{
  config = lib.mkIf cfg.enable {

    myhm = { ... }@hm: { };
  };
}
