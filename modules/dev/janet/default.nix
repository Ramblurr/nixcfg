{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  devCfg = config.modules.dev;
  cfg = devCfg.janet;
in
{
  options.modules.dev.janet = {
    enable = lib.mkEnableOption "";
  };

  config = mkIf cfg.enable {

    myhm = _: {

      home.packages = with pkgs; [
        janet
        jpm
      ];
    };
  };
}
