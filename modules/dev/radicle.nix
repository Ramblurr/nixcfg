{
  config,
  options,
  lib,
  pkgs,
  my,
  ...
}:
with lib;
with lib.my;
let
  devCfg = config.modules.dev;
  cfg = devCfg.radicle;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.dev.radicle = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home-manager.users."${username}" = {
      home.packages = [ inputs.radicle.packages.${pkgs.system}.default ];

      home.sessionVariables = {
        RAD_HOME = "${hm.config.xdg.configHome}/radicle";
      };
      home.persistence."/persist${homeDirectory}" = mkIf withImpermanence {
        directories = [ ".config/radicle" ];
      };
    };
  };
}
