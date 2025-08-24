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
  cfg = config.modules.shell.zellij;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.shell.zellij = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    myhm = {
      home.file.".config/zellij/config.kdl".source = ./config.kdl;
      home.packages = with pkgs; [ zellij ];
    };
  };
}
