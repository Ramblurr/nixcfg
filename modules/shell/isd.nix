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
  cfg = config.modules.shell.isd;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.shell.isd = {
    enable = lib.mkEnableOption "";
  };
  config = mkIf cfg.enable {
    environment.persistence."/persist" = lib.mkIf withImpermanence {
      users.${username} = {
        directories = [
          ".config/isd_tui"
          ".local/share/isd_tui"
          ".cache/isd_tui"
        ];
      };
    };
    myhm = {
      home.packages = [
        pkgs.isd
      ];
      xdg.configFile."isd_tui/config.yaml".text = builtins.toJSON {
        theme = "gruvbox";
      };
    };

  };
}
