{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  devCfg = config.modules.dev;
  cfg = devCfg.node;
  inherit (config.modules.users.primaryUser) username;
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
        deno
        #volta
      ];

      #xdg.configFile."npm" = {
      #  source = ./configs/npm;
      #  recursive = true;
      #};
    };
  };
}
