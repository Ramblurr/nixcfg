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
  cfg = devCfg.janet;
  username = config.modules.users.primaryUser.username;
  homeDirectory = config.modules.users.primaryUser.homeDirectory;
  withImpermanence = config.modules.impermanence.enable;
in
{
  options.modules.dev.janet = {
    enable = lib.mkEnableOption "";
  };

  config = mkIf cfg.enable {

    myhm =
      { ... }@hm:
      {

        home.packages = with pkgs; [
          janet
          jpm
        ];
      };
  };
}
