{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.programs.aseprite;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.desktop.programs.aseprite = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    myhm = {
      home.packages = with pkgs; [ pkgs.aseprite ];
      persistence = mkIf withImpermanence {
        directories = [
          ".config/libresprite"
          ".config/aseprite"
        ];
      };
    };
  };
}
