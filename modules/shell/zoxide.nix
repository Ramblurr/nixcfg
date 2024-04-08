{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.shell.zoxide;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.shell.zoxide = {
    enable = mkBoolOpt false;
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" = {
      programs.zoxide = {
        enable = true;
      };
      home.persistence."/persist${homeDirectory}" = mkIf withImpermanence {
        directories = [ ".local/share/zoxide" ];
      };
    };
  };
}
