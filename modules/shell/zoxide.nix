{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.shell.zoxide;
  inherit (config.modules.users.primaryUser) username;
  inherit (config.modules.users.primaryUser) homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.shell.zoxide = {
    enable = lib.mkEnableOption "";
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
