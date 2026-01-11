{
  config,
  lib,
  ...
}:

let
  cfg = config.modules.desktop.hyprland;

  #font.mono = "Iosevka Comfy Fixed";
  #font.mono = "Berkeley Mono Trial";
in

{
  config = lib.mkIf cfg.enable { };
}
