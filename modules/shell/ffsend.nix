{
  options,
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.shell.ffsend;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.shell.ffsend = {
    enable = lib.mkEnableOption "";
  };
  config = lib.mkIf cfg.enable {
    environment.persistence."/persist" = lib.mkIf withImpermanence {
      users.${username} = {
        directories = [
          ".cache/ffsend"
        ];
      };
    };
    systemd.tmpfiles.rules = lib.mkIf withImpermanence [
      "d '/persist${homeDirectory}/.cache/ffsend' - ${username} ${username} - -"
    ];
    home-manager.users."${username}" = {
      home.packages = [ pkgs.ffsend ];
    };
  };
}
