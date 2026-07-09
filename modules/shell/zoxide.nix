{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.shell.zoxide;
  inherit (config.modules.users.primaryUser) username;
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
    };
  };
}
