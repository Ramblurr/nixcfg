{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.shell.mpv;
  inherit (config.modules.users.primaryUser) username;
in
{
  options.modules.shell.mpv = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" = {
      programs.mpv = {
        enable = true;
        config = {
          #hwdec = "auto-safe";
          #vo = "gpu";
          #profile = "gpu-hq";
        };
        scripts = [
          pkgs.mpvScripts.uosc
          # pkgs.mpvScripts.cutter
          pkgs.mpvScripts.memo
          pkgs.mpvScripts.sponsorblock
          pkgs.mpvScripts.quality-menu
        ];
      };
    };
  };
}
