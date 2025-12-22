{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.modules.shell.ssh;
  inherit (config.modules.users.primaryUser) username;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.shell.ssh = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {

    environment.persistence."/persist" = mkIf withImpermanence {
      users.${username} = {
        directories = [ ".ssh" ];
      };
    };
    home-manager.users."${username}" = {
      # note using home-manager programs.ssh because of
      # https://github.com/nix-community/home-manager/issues/322
      home.file.".ssh/control/.keep".text = "";
    };
  };
}
