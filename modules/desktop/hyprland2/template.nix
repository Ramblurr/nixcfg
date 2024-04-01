{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.desktop.hyprland2;
in

{
  config = lib.mkIf cfg.enable {

    myhm = { ... }@hm: { };
  };
}
