{
  config,
  options,
  lib,
  pkgs,
  my,
  ...
}:
with lib;
let
  devCfg = config.modules.dev;
  cfg = devCfg.node;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.dev.node = {
    enable = lib.mkEnableOption "";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ ];

    environment.persistence."/persist" = mkIf withImpermanence {
      users.${username} = {
        directories = [
          ".config/npm"
          ".cache/npm-packages"
          ".config/yarn/"
          ".local/share/npm"
          ".local/share/volta"
        ];
      };
    };
    home-manager.users."${username}" = {
      home.packages = with pkgs; [
        nodejs_20
        yarn
        #volta
      ];

      #xdg.configFile."npm" = {
      #  source = ./configs/npm;
      #  recursive = true;
      #};
    };
  };
}
