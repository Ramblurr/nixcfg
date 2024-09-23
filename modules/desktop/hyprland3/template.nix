{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.desktop.hyprland3;
in

{
  config = lib.mkIf cfg.enable {

    myhm = { ... }@hm: { };
  };
}
