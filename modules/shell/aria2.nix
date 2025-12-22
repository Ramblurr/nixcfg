{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.shell.aria2;
  inherit (config.modules.users.primaryUser) username;
in
{
  options.modules.shell.aria2 = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    home-manager.users."${username}" = {
      programs.aria2 = {
        enable = true;
        settings = {
          listen-port = "6881-6999";
          dht-listen-port = "6881-6999";
        };
      };
    };
  };
}
